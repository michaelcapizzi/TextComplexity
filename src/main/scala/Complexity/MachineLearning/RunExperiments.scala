package Complexity.MachineLearning

import MLutils._
import edu.arizona.sista.learning._
import Scaling._

/**
  * Experiments to evaluate models using leave-one-out
  */
object RunExperiments {

  /**
    * @param args <br>
    *               args(0) = full path to dataset to be used
    *               args(1) = number of classes: `3` or `6`
    *               args(2+) = list of model types to use: `randomForest`, `perceptron`, `logisticRegression`, `svm`, or `all`
    */
  def main(args: Array[String]) = {

    /**
      * The dataset to be used
       */
    val dataset = importFromSVM(args(0))


    //normalize dataset values between 0 and 1
    normalizeData(dataset)


    /**
      * Models to be used in testing
      */
    val modelsToUse = args.slice(2, args.length)


    /**
      * Initialized model class for `randomForest`
      */
    var randomForestModel: Option[MLmodel] = None
    /**
      * Initialized classifier for `randomForest`
      */
    var randomForestClassifier: Option[RandomForestClassifier[Int, String]] = None
    /**
      * Initialized results variable for `randomForest`
      */
    var randomForestResults: Option[Vector[(Int, Int)]] = None

    /**
      * Initialized model class for `perceptron`
      */
    var perceptronModel: Option[MLmodel] = None
    /**
      * Initialized classfier for `perceptron`
      */
    var perceptronClassifier: Option[PerceptronClassifier[Int, String]] = None
    /**
      * Initialized results variable for `perceptron`
      */
    var perceptronResults: Option[Vector[(Int, Int)]] = None

    /**
      * Initialized model class for `logisticRegression`
      */
    var logisticRegressionModel: Option[MLmodel] = None
    /**
      * Initiailized classifier for `logisticRegression`
      */
    var logisticRegressionClassifier: Option[LogisticRegressionClassifier[Int, String]] = None
    /**
      * Initialized results variable for `logisticRegression`
      */
    var logisticRegressionResults: Option[Vector[(Int, Int)]] = None

    /**
      * Initialized model class for `svm`
      */
    var svmModel: Option[MLmodel] = None
    /**
      * Initialized classifier for `svm`
      */
    var svmClassifier: Option[LinearSVMClassifier[Int, String]] = None
    /**
      * Initialized results variable for `svm`
      */
    var svmResults: Option[Vector[(Int, Int)]] = None


    //populate appropriate models and classifiers and train/test (returning results in (gold, predicted) tuples)
    if (modelsToUse.contains("randomForest") || modelsToUse.contains("all")) {
      randomForestModel = Some(new MLmodel(dataset, "randomForest"))
      randomForestClassifier = Some(randomForestModel.get.classifier.asInstanceOf[RandomForestClassifier[Int, String]])
      randomForestResults = Some(Datasets.crossValidate[Int, String](
                                                                    dataset,
                                                                    () => randomForestClassifier.get,
                                                                    dataset.size).toVector)
    }

    if (modelsToUse.contains("perceptron") || modelsToUse.contains("all")) {
      perceptronModel = Some(new MLmodel(dataset, "perceptron"))
      perceptronClassifier = Some(perceptronModel.get.classifier.asInstanceOf[PerceptronClassifier[Int, String]])
      perceptronResults = Some(Datasets.crossValidate[Int, String](
                                                                    dataset,
                                                                    () => perceptronClassifier.get,
                                                                    dataset.size).toVector)
    }

    if (modelsToUse.contains("logisticRegression") || modelsToUse.contains("all")) {
      logisticRegressionModel = Some(new MLmodel(dataset, "logisticRegression"))
      logisticRegressionClassifier = Some(logisticRegressionModel.get.classifier.asInstanceOf[LogisticRegressionClassifier[Int, String]])
      logisticRegressionResults = Some(Datasets.crossValidate[Int, String](
                                                                            dataset,
                                                                            () => logisticRegressionClassifier.get,
                                                                            dataset.size).toVector)
    }

    if (modelsToUse.contains("svm") || modelsToUse.contains("all")) {
      svmModel = Some(new MLmodel(dataset, "svm", bias = Some(false)))
      svmClassifier = Some(svmModel.get.classifier.asInstanceOf[LinearSVMClassifier[Int, String]])
      svmResults = Some(Datasets.crossValidate[Int, String](
                                                            dataset,
                                                            () => svmClassifier.get,
                                                            dataset.size).toVector)
    }


    /**
      * List of possible sets of results
      */
    val results = Vector("Random Forest" -> randomForestResults, "Perceptron" -> perceptronResults, "Logistic Regression" -> logisticRegressionResults, "SVM" -> svmResults)




    //generate evaluation metrics
    for (r <- results.filter(_._2.nonEmpty)) {

      //model type
      val modelType = r._1

      //variable to capture the results as they are generated (gold, predicted)
      val resultsNoTitle = r._2.get

      //rebuild the tuple
      val resultsWithTitle= for (item <- resultsNoTitle) yield {
                                (
                                  "n/a",                                     //title placeholder
                                  revertLabel(item._2, args(1).toInt),    //predicted reverted to string
                                  revertLabel(item._1, args(1).toInt)     //gold reverted to string
                                )
      }

      //generate evalMetrics class
      val evalMetrics = new EvaluationMetrics(resultsWithTitle)

      println(modelType)
      println("-------------")
      println("accuracy: " + evalMetrics.accuracy.toString)
      println("macro precision: " + evalMetrics.macroScores("macroPrecision"))
      println("macro recall:" + evalMetrics.macroScores("macroRecall"))
      println("macroF1:" + evalMetrics.macroScores("macroF1"))
      println("=================")

    }

  }

}
