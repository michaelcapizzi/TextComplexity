package Complexity.MachineLearning

import MLutils._

object BuildModel {

  /**
    * Builds and saves a model (at the moment on RandomForest and LogisticRegression)
    * @param args <br>
    *               args(0) = classifier type: `randomForest`, `perceptron`, `logisticRegression`, or `svm`; defaults to `randomForest`
    *               args(1) = full path to saved feature matrix to load
    *               args(2) = full path for location for saved model
    *               args(3) = number of classes: `3` or `6`
    * @todo Add capacity to change model hyperparameters from here
    */
  def main(args: Array[String]) = {

    println("Loading saved dataset")

    val m = new MLmodel(
                          classifierType = args(0),
                          dataset = importFromSVM(args(1))
                        )


    println("scaling dataset")

//    m.normalizeDataset


    println("Training model")

    m.train


    println("Saving model")

    m.saveModel(args(2))



  }
}
