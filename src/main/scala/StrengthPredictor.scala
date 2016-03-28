import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.learning.EMWithVE
import com.cra.figaro.language.Apply
import com.cra.figaro.language.{Select, _}
import com.cra.figaro.library.atomic.continuous.Dirichlet
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.patterns.learning.ModelParameters

/** Inference for WiFi strengths
  */
object StrengthPredictor {

  /** Predicts a discrete distribution for WiFi strengths 0-99 based on one sensor
    *
    * @param sensor Sensor for which the the prediction is done. Should this method belong to Sensor class?
    * @param source WiFi signal source index
    * @return       List of probabilities, one for each value 0-99
    */
  def predict(sensor: Sensor, source: Int): Seq[Double] = {
    predict(Uniform(sensor), source)
  }

  /** Predicts a discrete distribution for WiFi strengths 0-99 based on a seq of sensors
    *
    * This isn't likely to be useful for the real problem and was created more as a middle step for simplifying the
    * development process. This assumes uniform distribution between the sensors.
    *
    * @param sensors  Sensors for which the the prediction is done
    * @param source   WiFi signal source index
    * @return         List of probabilities, one for each value 0-99
    */
  def predict(sensors: Seq[Sensor], source: Int): Seq[Double] = {
    predict(Uniform(sensors:_*), source)
  }

  /** Creates 100 Figaro elements, each for every strength value 0-99
    *
    * The created elements can then be used in different forms of inferences.
    * TODO: make sure 99 is the highest and not e.g. 100. Sample data never goes higher than 87 though.
    *
    * @param sensorDistribution Distribution for the sensors
    * @param source             WiFi source index
    * @return                   Sequence of Figaro elements for strength distribution inference
    */
  def getValueConditions(sensorDistribution: Select[Double, Sensor], source: Int): Seq[Element[Boolean]] = {
    val strengthDistribution = Chain(sensorDistribution, (s: Sensor) => s.distributions(source))
    for (value <- 0 until 100)
      yield Apply(strengthDistribution, (v: Double) => v >= value - 0.5 && v < value + 0.5)
  }

  /** Predicts WiFi strength distribution
    *
    * This is the method that should be used in the final implementation, together with sensor distribution prediction.
    *
    * @param sensorDistribution Distribution of the sensors
    * @param source             WiFi source index
    * @return                   List of probabilities, one for each value 0-99
    */
  def predict(sensorDistribution: Select[Double, Sensor], source: Int): Seq[Double] = {
    val valueConditions: Seq[Element[Boolean]] = getValueConditions(sensorDistribution, source)

    val alg = VariableElimination(valueConditions:_*)
    alg.start()
    alg.stop()

    val p = valueConditions.map(c => alg.probability(c, true))
    alg.kill()
    p
  }

  /** Finds a distribution for the sensors based on observations
    *
    * @param sensors      Sensors for which the the prediction is done
    * @param observations Observed values in a single observation. Index of the values should correspond with the source
    * @return             Sensor distribution as a Figaro Select element
    */
  def predictSensorDistribution(sensors: Seq[Sensor], observations: Seq[Int]): Select[Double, Sensor] = {

    val universe = Universe.createNew()

    val params = ModelParameters()
    Dirichlet(List.fill(sensors.size)(1.0):_*)("sensorDist", params)

    for ((o, sourceIndex) <- observations.zipWithIndex) {
      val sensorDist = Select(params.priorParameters.get("sensorDist"), sensors:_*)
      val strengthDistribution = Chain(sensorDist, (s: Sensor) => s.distributions(sourceIndex))
      Apply(strengthDistribution, (v: Double) => math.round(v)).observe(o)
    }

    val algorithm = EMWithVE(params)
    algorithm.start()
    algorithm.stop()
    algorithm.kill()

    Select(params.posteriorParameters.get("sensorDist"), sensors:_*)
  }

}
