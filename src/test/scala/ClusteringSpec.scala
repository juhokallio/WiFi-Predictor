import org.scalactic.TolerantNumerics
import org.scalatest._
/**
  * Created by juho on 3/23/16.
  */
class ClusteringSpec extends FlatSpec {

  val epsilon = 1e-4f

  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  val simpleStrengths = List(
    new Strengths(Array(1.0)),
    new Strengths(Array(1.1)),
    new Strengths(Array(3.0)),
    new Strengths(Array(3.1))
  )

  "findSensors" should "find two sensors when searching for two" in {
    assert(Clustering.findSensors(simpleStrengths, 2).size == 2)
  }

  "findSensors" should "give equal sized clusters" in {
    for(s <- Clustering.findSensors(simpleStrengths, 2))
      yield assert(s.sampleSize == 2)
  }

  "Sensor with one dimensional data" should "have only one distribution" in {
    val sensor = new Sensor(List(new Strengths(Array(1.0)), new Strengths(Array(2.0))))
    assert(sensor.distributions.size == 1)
    assert(sensor.dimensions == 1)
  }

  "findSensors" should "produce sensors with correct zero proportions" in {
    for(s <- Clustering.findSensors(simpleStrengths, 2))
      yield assert(s.zeroProportions.head == 0)
  }

  "findSensors" should "produce sensors with correct means" in {
    for(s <- Clustering.findSensors(simpleStrengths, 2))
      yield {
        val mean = s.nonZeroMeans.head
        assert(mean === 1.05 || mean === 3.05)
      }
  }

  "findSensors" should "produce sensors with correct variances" in {
    for(s <- Clustering.findSensors(simpleStrengths, 2))
      yield assert(s.nonZeroVariances.head === 0.0025)
  }
}
