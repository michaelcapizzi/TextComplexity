package Complexity.MachineLearning

import MLutils._
import edu.arizona.sista.learning._

/**
  * Experiments to evaluate models using leave-one-out
  */
object RunExperiments {

  /**
    * @param args <br>
    *               args(0) = full path to dataset to be used
    *               args(1+) = list of model types to use: `randomForest`, `perceptron`, `logisticRegression`, `svm`, or `all`
    */
  def main(args: Array[String]) = {

    /**
      * The dataset to be used
       */
    val dataset = importFromSVM(args(0))


    /**
      * Models to be used in testing
      */
    val modelsToUse = args.slice(1, args.length)


    /**
      * Initialized model class for `randomForest`
      */
    var randomForestModel: Option[MLmodel] = None
    var randomForestClassifier: Option[RandomForestClassifier[Int, String]] = None
    /**
      * Initialized model class for `perceptron`
      */
    var perceptronModel: Option[MLmodel] = None
    var perceptronClassifier: Option[PerceptronClassifier[Int, String]] = None
    /**
      * Initialized model class for `logisticRegression`
      */
    var logisticRegressionModel: Option[MLmodel] = None
    var logisticRegressionClassifier: Option[LogisticRegressionClassifier[Int, String]] = None
    /**
      * Initialized model class for `svm`
      */
    var svmModel: Option[MLmodel] = None
    var svmClassifier: Option[LinearSVMClassifier[Int, String]] = None

    //populate appropriate models and classifiers
    if (modelsToUse.contains("randomForest") || modelsToUse.contains("all")) {
      randomForestModel = Some(new MLmodel(dataset, "randomForest"))
      randomForestClassifier = Some(randomForestModel.get.classifier.asInstanceOf[RandomForestClassifier[Int, String]])
    }

    if (modelsToUse.contains("perceptron") || modelsToUse.contains("all")) {
      perceptronModel = Some(new MLmodel(dataset, "perceptron"))
      perceptronClassifier = Some(perceptronModel.get.classifier.asInstanceOf[PerceptronClassifier[Int, String]])
    }

    if (modelsToUse.contains("logisticRegression") || modelsToUse.contains("all")) {
      logisticRegressionModel = Some(new MLmodel(dataset, "logisticRegression"))
      logisticRegressionClassifier = Some(logisticRegressionModel.get.classifier.asInstanceOf[LogisticRegressionClassifier[Int, String]])
    }

    if (modelsToUse.contains("svm") || modelsToUse.contains("all")) {
      svmModel = Some(new MLmodel(dataset, "svm"))
      svmClassifier = Some(svmModel.get.classifier.asInstanceOf[LinearSVMClassifier[Int, String]])
    }






    //run leave-one-out tests



  }
}
