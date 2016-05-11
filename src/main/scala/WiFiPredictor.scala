import scala.util.Random

/** The main action of the system should happen here
  *
  * Best score with old normal distributions: -21417.37574075611
  * Best score with discrete distribution: -14883.096604932123
  *
  * Target minimum score: -22700.896102278213
  */
object WiFiPredictor {

  /** The number of samples that are taken out of training data and used for testing the accuracy */
  val TEST_DATA_SIZE = 1

  /** The number of sensors are assumed to be in the data. Ideally this would be chosen with some sort of CV */
  val SENSOR_COUNT = 100

  /** The number of observations used when running the system
    *
    * The CSV file contains 94671 observation samples, which is too much for development use (at least with the currect
    * performance).
    */
  val DATA_SIZE_LIMIT = 8000

  /** Loads the CSV WiFi strength observation data to form that the rest of the system can work with */
  def loadData(): List[Strengths] = {
    val bufferedSource = io.Source.fromFile("data/wifisensors.csv")
    val observations = (for (line <- bufferedSource.getLines)
      yield new Strengths(line.split(",").map(_.toInt)))
        .toList
    bufferedSource.close
    observations
  }
  
  def evaluate(sensors: Seq[(Double, Sensor)], observation: Strengths, oIndex: Int): Double = {
    if (oIndex == observation.size)
      return 0
    val o: Observation = Observation(oIndex, observation.getStrength(oIndex))
    val newDist: Seq[(Double, Sensor)] = StrengthPredictor.predictSensorDistribution(sensors, o)
    val correct = observation.getStrength(oIndex)
    val likelihood: Double = StrengthPredictor.predict(newDist, oIndex)(correct)
    println (oIndex + " p: " + Math.pow(10, likelihood))
    likelihood + evaluate(newDist, observation, oIndex + 1)
  }

  def evaluate(sensors: Seq[(Double, Sensor)], observation: Strengths, oIndex: Int, outputs: Seq[String]): Double = {
    if (oIndex == observation.size)
      return 0
    val o: Observation = Observation(oIndex, observation.getStrength(oIndex))
    val newDist: Seq[(Double, Sensor)] = StrengthPredictor.predictSensorDistribution(sensors, o)
    val correct = observation.getStrength(oIndex)
    val likelihood: Double = StrengthPredictor.predict(newDist, oIndex)(correct)
    println (oIndex + " p: " + Math.pow(10, likelihood))
    likelihood + evaluate(newDist, observation, oIndex + 1)
  }

  def test() {
    val t0 = System.currentTimeMillis()
    println("Starting WiFi predictor")
    val observations = Random.shuffle(loadData()).take(DATA_SIZE_LIMIT)
    val t1 = System.currentTimeMillis()
    println("\nData loaded and shuffled in " + (t1 - t0) / 1000 + "s")
    val (trainingData, testData) = observations.splitAt(observations.size - TEST_DATA_SIZE)
    val t2 = System.currentTimeMillis()
    println("\nData split to training and test data in " + (t2 - t1) + "ms")
    println("\tTraining data size:\t" + trainingData.size)
    println("\tTest data size:\t" + testData.size)
    val sensors = Clustering.findSensors(trainingData, SENSOR_COUNT)
    val t3 = System.currentTimeMillis()
    println("\nClustering done, " + sensors.size + " sensors found in " + (t3 - t2) / 1000 + "s")
    val t4 = System.currentTimeMillis()
    val initialDistribution: Seq[(Double, Sensor)] = sensors.map(s => (0.0, s))
    val logLikelihood = testData.map(evaluate(initialDistribution, _, 0))
    println("\nTesting done with " + testData.size + " samples in " + (t4 - t3) / 1000 + "s")
    println("\tTotal loglikelihood was " + logLikelihood)

  }

  def process(sensors: Seq[(Double, Sensor)], observation: Strengths): Unit = {
    val output: String = StrengthPredictor.predict(sensors, observation.size)
      .map(ll => Math.pow(10, ll))
      .mkString(",")
    println(output)
    val input: String = io.StdIn.readLine()
    if (input.equals("n"))
      return
    val o: Observation = Observation(observation.size, Integer.parseInt(input))
    val newDist: Seq[(Double, Sensor)] = StrengthPredictor.predictSensorDistribution(sensors, o)

    if (observation.size < 302)
      process(newDist, observation + o)
    else
      process(sensors.map{case (_, s) => (0.0, s)}, new Strengths(Array.emptyIntArray))
  }

  def main(params: Array[String]) {
    val trainingData = Random.shuffle(loadData())
    val sensors = Clustering.findSensors(trainingData, SENSOR_COUNT)
    val initialDistribution: Seq[(Double, Sensor)] = sensors.map(s => (0.0, s))
    process(initialDistribution, new Strengths(Array.emptyIntArray))
  }
}
