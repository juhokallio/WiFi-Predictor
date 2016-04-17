
case class Observation(sensorIndex: Int, value: Int)

/** Inference for WiFi strength
  */
object StrengthPredictor {


  /** Predicts WiFi strength distribution
    *
    * This is the method that should be used in the final implementation, together with sensor distribution prediction.
    *
    * @param sensors Distribution of the sensors
    * @param source             WiFi source index
    * @return                   List of probabilities, one for each value 0-99
    */
  def predict(sensors: Seq[(Double, Sensor)], source: Int): Seq[Double] = {
    val probabilities: Seq[Seq[Double]] = for ((prior, s) <- sensors)
      yield predictProbability(s, prior, source)
    val mass: Double = probabilities.flatten.sum

    val ps: Seq[Double] = (probabilities foldLeft Array.fill(100)(0.0)) {
          (a1, a2) => (a1, a2).zipped.map { case (v1, v2) => v1 + v2 / mass }
        }

    ps.map(v => Math.log10(v))
  }

  def predictProbability(sensor: Sensor, prior: Double, source: Int): Seq[Double] = {
    sensor.logLikelihoods(source)
      .map(l => l + prior)
      .map(l => Math.pow(10, l))
  }

  /** Finds a distribution for the sensors based on observations
    *
    * @param sensors      Sensors for which the the prediction is done
    * @param observations Observed values in a single observation. Index of the values should correspond with the source
    * @return             Sensor distribution as a Figaro Select element
    */
  def predictSensorDistribution(sensors: Seq[(Double, Sensor)], observation: Observation): Seq[(Double, Sensor)] = {
    for ((prior, s) <- sensors)
      yield (prior + s.logLikelihood(observation), s)
  }

}
