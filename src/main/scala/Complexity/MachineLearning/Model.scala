package Complexity.MachineLearning

import edu.arizona.sista.learning._
import edu.arizona.sista.struct.Counter

/**
  *
  * @param classifierType `randomForest`, `perceptron`, `logisticRegression`, or `svm`; defaults to `randomForest`
  * @param RFnumTrees For `randomForest`: Number of trees
  * @param RFfeatureSampleRatio For `randomForest`: Sample ratio
  * @param RFmaxTreeDepth For `randomForest`: Maximum tree depth
  * @param RFnumThreads For `randomForest`: Number of threads
  * @param Pepochs For `perceptron`: Number of epochs
  * @param PburnIn For `perceptron`: Burn-in epochs
  * @param bias For `logisticRegression` and `svm`: Include bias?
  */
class Model(
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

    case "randomForest" => new RandomForestClassifier[String, String](
                            numTrees = RFnumTrees.getOrElse(1000),
                            featureSampleRatio = RFfeatureSampleRatio.getOrElse(1.0),
                            maxTreeDepth = RFmaxTreeDepth.getOrElse(5),
                            numThreads = RFnumThreads.getOrElse(3)
                        )
    case "perceptron" => new PerceptronClassifier[String, String](
                            epochs = Pepochs.getOrElse(20),
                            burnInIterations = PburnIn.getOrElse(1)
                        )
    case "logisticRegression" => new LogisticRegressionClassifier[String, String](
                            bias = bias.getOrElse(true)
                        )
    case "svm" => new LinearSVMClassifier[String, String](
                            bias = bias.getOrElse(true)
                        )
    case _ => new RandomForestClassifier[String, String](
                            numTrees = RFnumTrees.getOrElse(1000),
                            featureSampleRatio = RFfeatureSampleRatio.getOrElse(1.0),
                            maxTreeDepth = RFmaxTreeDepth.getOrElse(5),
                            numThreads = RFnumThreads.getOrElse(3)
                        )
  }

  /**
    * Variable to contain the dataset
    */
  var dataset = new RVFDataset[String, String]()


  /**
    * Builds a dataset from given list of Datums
    *
    * @param datumSeq List of Datums
    */
  def makeDataset(datumSeq: Vector[RVFDatum[String, String]]): Unit = {
    datumSeq.foreach(this.dataset.+=(_))
  }


  /**
    * Trains the dataset
    */
  def train: Unit = {
    this.classifier.train(this.dataset)
  }


  /**
    * Produces predictions for a given test set
    * @param titles `Vector` of titles to be tested
    * @param datumSeq `Vector` of `Datum`s to be tested
    * @return `Vector` of tuples of `(title, predictedScore, actualScore)`
    */
  def test(titles: Vector[String], datumSeq: Vector[RVFDatum[String, String]]): Vector[(String, String, String)] = {
    (
      titles,                                             //title
      datumSeq.map(d => this.classifier.classOf(d)),      //predicted (mL) score
      datumSeq.map(d => d.label)                          //actual score
    ).zipped.toVector
  }


  /**
    * Produces predictions for a given test set
    * @param titles `Vector` of titles to be tested
    * @param datumSeq `Vector` of `Datum`s to be tested
    * @return `Vector` of tuples of `(title, predictedScore, actualScore)` with a `Counter` of confidences
    */
  def testWithConfidence(titles: Vector[String], datumSeq: Vector[RVFDatum[String, String]]): Vector[((String, String, String), Counter[String])] = {
    this.test(titles, datumSeq).zip(                                //results from test method
                                    datumSeq.map(d =>
                                      this.classifier.scoresOf(d)   //confidence values
                                      )
                                    )
  }


}
