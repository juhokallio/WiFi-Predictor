import com.cra.figaro.language.Select
import org.scalactic.TolerantNumerics
import org.scalatest.FlatSpec

/**
  * Created by juho on 3/26/16.
  */
class SensorDistributionSpec extends FlatSpec {

  val epsilon = 1e-4f

  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  "with no observations sensors" should "be equally likely" in {
    val s1 = new Sensor(List(new Strengths(Array(0))))
    val s2 = new Sensor(List(new Strengths(Array(99))))
    val predicted: Select[Double, Sensor] = StrengthPredictor.predictSensorDistribution(List(s1, s2), List())
    for ((p, s) <- predicted.clauses) assert(p === 0.5)
  }

  "sensor with matching observations" should "be very likely" in {
    val s1 = new Sensor(List(new Strengths(Array(0, 0, 0, 0, 0, 0))))
    val s2 = new Sensor(List(new Strengths(Array(99, 99, 99, 99, 99, 99))))
    val predicted: Select[Double, Sensor] = StrengthPredictor.predictSensorDistribution(List(s1, s2), List(0, 0, 0, 0, 0, 0))
    for ((p, s) <- predicted.clauses) s match {
      case `s1` => assert(p === 1.0)
      case `s2` => assert(p === 0.0)
    }
  }
}
