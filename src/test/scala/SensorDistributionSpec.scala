import org.scalactic.TolerantNumerics
import org.scalatest.FlatSpec

/**
  * Created by juho on 3/26/16.
  */
class SensorDistributionSpec extends FlatSpec {

  val epsilon = 1e-4f

  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

}
