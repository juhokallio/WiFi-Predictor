import com.cra.figaro.language.Select

import scala.util.Random

/** The main action of the system should happen here
  */
object WiFiPredictor {

  /** The number of samples that are taken out of training data and used for testing the accuracy */
  val TEST_DATA_SIZE = 1

  /** The number of sensors are assumed to be in the data. Ideally this would be chosen with some sort of CV */
  val SENSOR_COUNT = 3

  /** The number of observations used when running the system
    *
    * The CSV file contains 94671 observation samples, which is too much for development use (at least with the currect
    * performance).
    */
  val DATA_SIZE_LIMIT = 20

  /** Loads the CSV WiFi strength observation data to form that the rest of the system can work with */
  def loadData(): List[Strengths] = {
    val bufferedSource = io.Source.fromFile("data/wifisensors.csv")
    val observations = (for (line <- bufferedSource.getLines)
      yield new Strengths(line.split(",").map(_.toInt)))
        .toList
    bufferedSource.close
    observations
  }

  /** Evaluates an observation
    *
    * In real case only observation values lower than source index would be available, so those are the visible
    * observations which let inferring the posterior distribution for sensors. The function returns the log likelihood
    * of the sourceIndex element of the observation.
    *
    * @param sensors      Sensors that are present in the underlying model
    * @param observation  Whole observation of some sensor
    * @param sourceIndex  Index of the source to be evaluated
    * @return             Loglikelihood of the observation
    */
  def evaluate(sensors: Seq[Sensor], observation: Strengths, sourceIndex: Int): Double = {
    val visibleObservations: Seq[Int] = observation.getPoint.take(sourceIndex).map(_.toInt)
    val sensorDistribution = StrengthPredictor.predictSensorDistribution(sensors, visibleObservations)
    val valueDistribution: Seq[Double] = StrengthPredictor.predict(sensorDistribution, sourceIndex)
    Math.log10(valueDistribution(observation.getStrength(sourceIndex)))
  }

  /** Finds the loglikelihood of the given observation */
  def evaluate(sensors: Seq[Sensor], observation: Strengths): Double = (0 until observation.size)
      .map(evaluate(sensors, observation, _)).sum

  def main(params: Array[String]) {
    val t0 = System.currentTimeMillis()
    println("Starting WiFi predictor")
    val observations = Random.shuffle(loadData().take(DATA_SIZE_LIMIT))
    val t1 = System.currentTimeMillis()
    println("\nData loaded and shuffled in " + (t1 - t0) / 1000 + "s")
    val (trainingData, testData) = observations.splitAt(observations.size - TEST_DATA_SIZE)
    val t2 = System.currentTimeMillis()
    println("\nData split to training and test data in " + (t2 - t1) + "ms")
    println("\tTraining data size:\t" + trainingData.size)
    println("\tTest data size:\t" + testData.size)
    val sensors = Clustering.findSensors(trainingData, SENSOR_COUNT)
    val t3 = System.currentTimeMillis()
    println("\nClustering done, " + sensors.size + " sensors found in " + (t3 - t2) / 1000 + "s")
    val t4 = System.currentTimeMillis()
    val logLikelihood = testData.map(evaluate(sensors, _)).sum
    println("\nTesting done with " + testData.size + " samples in " + (t4 - t3) / 1000 + "s")
    println("\tTotal loglikelihood was " + logLikelihood)

  }
}
