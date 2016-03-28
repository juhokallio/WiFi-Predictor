import scala.util.Random

/** The main action of the system should happen here
  */
object WiFiPredictor {

  /** The number of samples that are taken out of training data and used for testing the accuracy */
  val TEST_DATA_SIZE = 100

  /** The number of sensors are assumed to be in the data. Ideally this would be chosen with some sort of CV */
  val SENSOR_COUNT = 100

  /** The number of observations used when running the system
    *
    * The CSV file contains 94671 observation samples, which is too much for development use (at least with the currect
    * performance).
    */
  val DATA_SIZE_LIMIT = 2000

  /** Loads the CSV WiFi strength observation data to form that the rest of the system can work with */
  def loadData(): List[Strengths] = {
    val bufferedSource = io.Source.fromFile("data/wifisensors.csv")
    val observations = (for (line <- bufferedSource.getLines)
      yield new Strengths(line.split(",").map(_.toInt)))
        .toList
    bufferedSource.close
    observations
  }

  def main(params: Array[String]) {
    val t0 = System.currentTimeMillis()
    println("Starting WiFi predictor")
    val observations = Random.shuffle(loadData().take(DATA_SIZE_LIMIT))
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
  }
}
