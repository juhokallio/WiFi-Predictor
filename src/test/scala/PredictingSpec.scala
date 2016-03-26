import org.scalactic.TolerantNumerics
import org.scalatest._

/**
  * Created by juho on 3/25/16.
  */
class PredictingSpec extends FlatSpec {

  val epsilon = 1e-4f

  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  "predict" should "return 100 length array" in {
    val observation = new Strengths(Array(1.0))
    val sensor = new Sensor(List(observation))
    val predicted = StrengthPredictor.predict(sensor, 0)
    assert(predicted.length == 100)
  }

  "predicted array sum" should "be one" in {
    val observation = new Strengths(Array(2.0, 1.2))
    val sensor = new Sensor(List(observation))
    val predicted = StrengthPredictor.predict(sensor, 0)
    assert(predicted.sum === 1.0)
  }

  "predicted array zero" should "be near zero with high signal source" in {
    val observation = new Strengths(Array(99.0))
    val sensor = new Sensor(List(observation, observation))
    val predicted = StrengthPredictor.predict(sensor, 0)
    assert(predicted(0) === 0.0)
  }

  "predicted array zero" should "be near one with dead signal source" in {
    val observation = new Strengths(Array(0.0))
    val sensor = new Sensor(List(observation, observation))
    val predicted = StrengthPredictor.predict(sensor, 0)
    assert(predicted(0) === 1.0)
  }

  "predict with list param" should "return 100 length array" in {
    val observation = new Strengths(Array(1.0))
    val sensor = new Sensor(List(observation))
    val predicted = StrengthPredictor.predict(List(sensor), 0)
    assert(predicted.length == 100)
  }

  "predicted array sum with list param" should "be one" in {
    val observation = new Strengths(Array(2.0, 1.2))
    val sensor = new Sensor(List(observation))
    val predicted = StrengthPredictor.predict(List(sensor), 0)
    assert(predicted.sum === 1.0)
  }

  "predicted array zero with list param" should "be near zero with high signal source" in {
    val observation = new Strengths(Array(99.0))
    val sensor = new Sensor(List(observation, observation))
    val predicted = StrengthPredictor.predict(List(sensor), 0)
    assert(predicted(0) === 0.0)
  }

  "predicted array zero with list param" should "be near one with dead signal source" in {
    val observation = new Strengths(Array(0.0))
    val sensor = new Sensor(List(observation, observation))
    val predicted = StrengthPredictor.predict(List(sensor), 0)
    assert(predicted(0) === 1.0)
  }
}
