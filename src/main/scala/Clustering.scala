import org.apache.commons.math3.ml.clustering.{Clusterable, KMeansPlusPlusClusterer}
import org.apache.commons.math3.distribution.NormalDistribution

import collection.JavaConversions._
import scala.collection.mutable

/** WiFi strength observations. Implements the Clusterable Java interface, which is needed for Apache Commons
  * clustering methods.
  *
  * @param data Array of WiFi source strengths, 0-100
  */
class Strengths(data: Array[Int]) extends Clusterable  {

  /** The number of separate WiFi sources that have been measured for strengths */
  val size = data.length

  /** Returns the WiFi source strength data. Needed for Apache Commons clustering. */
  override def getPoint: Array[Double] = data.map(s => s.toDouble)

  /** Gets the Wifi strength of the given source device index */
  def getStrength(source: Int): Int = data(source)

  def +(o: Observation): Strengths = new Strengths(data :+ o.value)

  def apply(): Strengths = new Strengths(Array.emptyIntArray)
}

/** Representation of a WiFi sensor
  *
  * The distribution of single Wifi source's strenghts is expected to be normally distributed, except when it's offline.
  * For this reason, data is modelled with two distributions:
  * 1.) normal distribution for non-zero strength, and
  * 2.) constant for probability of strength being zero.
  *
  * @param fittingData  WiFi strength observation data of this sensor
  */
class Sensor(fittingData: List[Strengths]) {

  val SMOOTHING: Double = 1.0 / (dimensions *(sampleSize + 1))

  /** Number of dimensions in one strength observation, e.g. how many separate WiFi sources are considered for strengths */
  lazy val dimensions: Int = fittingData.head.size

  /** Size of the samples used to infer the sensor information */
  lazy val sampleSize: Int = fittingData.size

  /** The strength distributions [0, 100] of the different WiFi sources
    *
    * TODO: Not quite exact, as the whole gaussian won't fit ]0, 100] like this.
    */
  val distributions: Seq[NormalDistribution] = for (source <- 0 until dimensions)
    yield new NormalDistribution(nonZeroMeans(source), math.max(nonZeroVariances(source), 1.0))

  /** The variance of the data points that aren't zero - zeroes are expected to form a separate spike */
  lazy val nonZeroVariances: Seq[Double] = for (source <- 0 until dimensions) yield {
    // With n = 1 variance is undefined and yet we want to have there something, so... Could think this a bit more.
    val n = math.max(sampleSize * (1 - zeroProportions(source)), 2)
    fittingData.map(s => s.getStrength(source))
      .filter(s => s != 0)
      .map(s => (nonZeroMeans(source) - s) * (nonZeroMeans(source) - s))
      .sum / (n - 1)
  }

  /** The variance of the data points that aren't zero */
  lazy val nonZeroMeans: Seq[Double] = for (source <- 0 until dimensions)
    yield {
      val normalProportion = 1 - zeroProportions(source)
      normalProportion match {
        case 0 => -10
        case _ => fittingData.map(s => s.getStrength(source))
          .filter(s => s != 0)
          .sum / (sampleSize * normalProportion)
      }
    }

  /** The zero strength probabilities of each of the WiFi source */
  lazy val zeroProportions: Seq[Double] = for (source <- 0 until dimensions)
    yield fittingData.count(s => s.getStrength(source) == 0)
      .toDouble / fittingData.size

  def logLikelihoods(source: Int): Seq[Double] = (0 to 100)
    .map(v => if(v == 0) zeroProportions(source) else distributions(source).probability(v - 0.5, v + 0.5))
    .map(v => Math.log10(math.max(SMOOTHING, v)))

  def logLikelihood(observation: Observation) = {
    logLikelihoods(observation.sensorIndex)(observation.value)
  }

}

/** WiFi sensor discovery functionality
  *
  * The WiFi data that is targeted doesn't contain any information about the sensors. The sensors could be considered
  * as hidden variables, but here they are attempted to be reproduced explicitly with clustering. Apache Commons
  * k-means++ implementation is used, mainly for getting something done fast and simply.
  */
object Clustering {

  /** Finds the sensors from given data
    *
    * @param data List of WiFi strength observations
    * @param k    Number of sensors
    * @return     Sensors
    */
  def findSensors(data: List[Strengths], k: Int): mutable.Buffer[Sensor] = new KMeansPlusPlusClusterer[Strengths](k, 20)
      .cluster(data)
      .map(c => new Sensor(c.getPoints.toList))
}
