import org.scalactic.TolerantNumerics
import org.scalatest.FlatSpec

/**
  * Created by juho on 3/27/16.
  */
class SensorSpec extends FlatSpec {

  val epsilon = 1e-4f

  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  "Sensor with one dimensional data" should "have only one distribution" in {
    val sensor = new Sensor(List(new Strengths(Array(1)), new Strengths(Array(2))))
    assert(sensor.distributions.size == 1)
    assert(sensor.dimensions == 1)
  }

  "Sensor with non zero values" should "have ~empty zero proportion" in {
    val sensor = new Sensor(List(new Strengths(Array(60)), new Strengths(Array(62))))
    assert(sensor.zeroProportions.head === 0.0)
  }

  "Sensor" should "have correct variances with two data points" in {
    val sensor = new Sensor(List(new Strengths(Array(1)), new Strengths(Array(2))))
    assert(sensor.nonZeroVariances.head == 0.5)
  }
  "Sensor" should "have correct variances with three data points" in {
    val sensor = new Sensor(List(new Strengths(Array(1)), new Strengths(Array(2)), new Strengths(Array(2))))
    assert(sensor.nonZeroVariances.head === 0.3333)
  }
}
