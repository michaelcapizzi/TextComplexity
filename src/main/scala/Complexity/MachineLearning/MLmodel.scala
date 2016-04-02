package Complexity.MachineLearning

import edu.arizona.sista.learning._
import edu.arizona.sista.struct.Counter
import MLutils._
import Scaling._
import Complexity.Utils.Compress._


/**
  * Generates the model and datasets for training and testing
  *
  * @param dataset Can be initiated as empty and built using `makeDataset`, or can be included at instantiation
  * @param classifierType `randomForest`, `perceptron`, `logisticRegression`, or `svm`; defaults to `randomForest`
  * @param RFnumTrees For `randomForest`: Number of trees
  * @param RFfeatureSampleRatio For `randomForest`: Sample ratio
  * @param RFmaxTreeDepth For `randomForest`: Maximum tree depth
  * @param RFnumThreads For `randomForest`: Number of threads
  * @param Pepochs For `perceptron`: Number of epochs
  * @param PburnIn For `perceptron`: Burn-in epochs
  * @param bias For `logisticRegression` and `svm`: Include bias?
  */
class MLmodel(
            var dataset: RVFDataset[Int, String] = new RVFDataset[Int, String](),
            val classifierType: String,
            val RFnumTrees: Option[Int] = None,
            val RFfeatureSampleRatio: Option[Double] = None,
            val RFmaxTreeDepth: Option[Int] = None,
            val RFnumThreads: Option[Int] = None,
            val Pepochs: Option[Int] = None,
            val PburnIn: Option[Int] = None,
            val bias: Option[Boolean] = None
           ) {

  /**
    * Variable to contain classifier, defaults to `RandomForestClassifier`
    */
  var classifier = this.classifierType match {

    case "randomForest" => new RandomForestClassifier[Int, String](
                            numTrees = RFnumTrees.getOrElse(100),
                            featureSampleRatio = RFfeatureSampleRatio.getOrElse(1.0),
                            maxTreeDepth = RFmaxTreeDepth.getOrElse(5),
                            numThreads = RFnumThreads.getOrElse(0)
                        )
    case "perceptron" => new PerceptronClassifier[Int, String](
                            epochs = Pepochs.getOrElse(20),
                            burnInIterations = PburnIn.getOrElse(1)
                        )
    case "logisticRegression" => new LogisticRegressionClassifier[Int, String](
                            bias = bias.getOrElse(true)
                        )
    case "svm" => new LinearSVMClassifier[Int, String](
                            bias = bias.getOrElse(true)
                        )
    case _ => new RandomForestClassifier[Int, String](
                            numTrees = RFnumTrees.getOrElse(1000),
                            featureSampleRatio = RFfeatureSampleRatio.getOrElse(1.0),
                            maxTreeDepth = RFmaxTreeDepth.getOrElse(5),
                            numThreads = RFnumThreads.getOrElse(0)
                        )
  }


  /**
    * Builds a dataset (if none was given at class instantiation) from list of Datums
    *
    * @param datumSeq List of Datums
    */
  def makeDataset(datumSeq: Vector[RVFDatum[Int, String]]): Unit = {
    datumSeq.foreach(this.dataset.+=(_))
  }


  /**
    * Normalizes all feature values between 0 and 1 using [[edu.arizona.sista.learning.Datasets.svmScaleDataset]]
    * @return Range to be used as input to [[Complexity.MachineLearning.Scaling.normalizeDatum]]
    */
  def normalizeDataset: ScaleRange[String] = {
    normalizeData(this.dataset)
  }


  /**
    * Trains the dataset
    * @todo Because of the different options for classifier in [[classifier]], must use `asInstanceOf...` before running training.  Find alternative fix
    */
  def train: Unit = {
    //set correct instance of variable
    this.classifierType match {
      case "randomForest" => this.classifier = this.classifier.asInstanceOf[RandomForestClassifier[Int, String]]
      case "perceptron" => this.classifier = this.classifier.asInstanceOf[PerceptronClassifier[Int, String]]
      case "logisticRegression" => this.classifier = this.classifier.asInstanceOf[LogisticRegressionClassifier[Int, String]]
      case "svm" => this.classifier = this.classifier.asInstanceOf[LinearSVMClassifier[Int, String]]
      case _ => this.classifier = this.classifier.asInstanceOf[RandomForestClassifier[Int, String]]
    }

    //run training
    this.classifier.train(this.dataset)

  }

  /**
    * Produces predictions for a given test set
    *
    * @param titles `Vector` of titles to be tested
    * @param datumSeq `Vector` of `Datum`s to be tested
    * @return `Vector` of tuples of `(title, predictedScore, actualScore)`
    */
  def test(titles: Vector[String], datumSeq: Vector[RVFDatum[Int, String]], numClasses: Int): Vector[(String, String, String)] = {
    (
      titles,                                             //title
      datumSeq.map(d => revertLabel(this.classifier.classOf(d), numClasses)),      //predicted (mL) score (reverted to string)
      datumSeq.map(d => revertLabel(d.label, numClasses))                          //actual score (reverted to string)
    ).zipped.toVector
  }


  /**
    * Produces predictions for a given test set
    *
    * @param titles `Vector` of titles to be tested
    * @param datumSeq `Vector` of [[edu.arizona.sista.learning.Datum]]s to be tested
    * @return `Vector` of tuples of `(title, predictedScore, actualScore)` with a [[edu.arizona.sista.struct.Counter]] of confidences (still in converted Integer format)
    */
  def testWithConfidence(titles: Vector[String], datumSeq: Vector[RVFDatum[Int, String]], numClasses: Int): Vector[((String, String, String), Counter[Int])] = {
    this.test(titles, datumSeq, numClasses).zip(                                //results from test method
                                                datumSeq.map(d =>
                                                        this.classifier.scoresOf(d)   //confidence values
                                                )
                                            )
  }


  /**
    * Make prediction for a single datum
    * @param datum The [[edu.arizona.sista.learning.Datum]] to be predicted
    * @param numClasses `3` or `6`
    * @return Tuple `(prediction, Counter[confidences], Counter[feature values])`
    */
  def predict(datum: RVFDatum[Int, String], numClasses: Int): (String, Counter[String], Counter[String]) = {

    //new counter (to house reverted labels)
    val updatedConfidences = new Counter[String]()
    //original confidences
    val originalConfidences = this.classifier.scoresOf(datum)
    //update new counter with reverted labels
    for (k <- originalConfidences.keySet) {
                        updatedConfidences.setCount(
                                  revertLabel(k, numClasses),       //reverted label
                                  originalConfidences.getCount(k)   //original confidence value
                        )
    }

    //get prediction
    val prediction = revertLabel(this.classifier.classOf(datum), numClasses)

    //get feature counter
    val features = datum.featuresCounter

    (
      prediction,
      updatedConfidences,
      features
    )
  }


  /**
    * Save trained model to file
    *
    * @param fileName Location for trained model
    */
  def saveModel(fileName: String): Unit = {
    this.classifier.saveTo(fileName)
  }


  /**
    * Load trained model from file <br>
    *   Only `randomForest` and `perceptron` can be loaded from a saved state
    *   Model can be compressed with `.gz` for `perceptron` and then requires use of [[Complexity.Utils.Compress]]
    *
    * @param fileName uncompressed or compressed file location written from [[resources/]]
    */
  def loadModel(fileName: String): Unit = {
    this.classifierType match {
      case "randomForest" => this.classifier = RandomForestClassifier.loadFrom(fileName)
      //      case "perceptron" => this.classifier = PerceptronClassifier.loadFrom(unGZmlModel(fileName))
      case "perceptron" => this.classifier = PerceptronClassifier.loadFrom(fileName)
      //      case "logisticRegression" => this.classifier = LogisticRegressionClassifier.loadFrom(unGZmlModel(fileName))
      //      case "svm" => this.classifier = LinearSVMClassifier.loadFrom(unGZmlModel(filName))
    }
  }

}
