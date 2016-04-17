import org.scalactic.TolerantNumerics
import org.scalatest._

/**
  * Created by juho on 3/25/16.
  */
class PredictingSpec extends FlatSpec {

  val epsilon = 1e-4f

  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)


}
