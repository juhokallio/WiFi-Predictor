import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.Apply
import com.cra.figaro.language._

/** Inference for WiFi strengths
  */
object StrengthPredictor {

  /** Predicts a discrete distribution for WiFi strengths 0-99
    *
    * TODO: make sure 99 is the highest and not e.g. 100. Sample data never goes higher than 87 though.
    *
    * @param sensor Sensor for which the the prediction is done. Should this method belong to Sensor class?
    * @param source WiFi signal source index
    * @return       List of probabilities, one for each value 0-99
    */
  def predict(sensor: Sensor, source: Int): Seq[Double] = {
    val dist: Element[Double] = sensor.distributions(source)
    val valueConditions: Seq[Element[Boolean]] = for (value <- 0 until 100)
      yield Apply(dist, (v: Double) => v >= value - 0.05 && v < value + 0.05)

    val alg = VariableElimination(valueConditions:_*)
    alg.start()
    alg.stop()

    val p: Seq[Double] = for (c <- valueConditions)
      yield alg.probability(c, true)
    alg.kill()
    p
  }

}
