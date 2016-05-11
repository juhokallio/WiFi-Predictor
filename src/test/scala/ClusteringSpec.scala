import org.scalactic.TolerantNumerics
import org.scalatest._
/**
  * Created by juho on 3/23/16.
  */
class ClusteringSpec extends FlatSpec {

  val epsilon = 1e-4f

  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  val simpleStrengths = List(
    new Strengths(Array(1)),
    new Strengths(Array(2)),
    new Strengths(Array(10)),
    new Strengths(Array(11))
  )

  "findSensors" should "find two sensors when searching for two" in {
    assert(Clustering.findSensors(simpleStrengths, 2).size == 2)
  }

  "findSensors" should "give equal sized clusters" in {
    for (s <- Clustering.findSensors(simpleStrengths, 2))
      assert(s.sampleSize == 2)
  }

}
