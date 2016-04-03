package Complexity

import MachineLearning.MLutils._

/**
  * Uses best model configuration for each class structure
  */
object Demo {

  /**
    *
    * @param args args(0) = file to test
    *             args(1) = number of classes `3` or `6`
    */
  def main(args: Array[String]) = {

    if (args(1) == "3") {
      //calls Predict Main class but uses best parameters: svm, lexical and paragraph features
      Predict.main(
        Array[String](
          args(0),
          "3",
          "svm",
          getClass.getResource("/savedFeatureMatrices/lex_par-3.svmLight").getPath,
          "lexical",
          "paragraph"
        )
      )
    } else if (args(1) == "6") {
      //calls Predict Main class but uses best parameters: random forest, lexical features
      Predict.main(
        Array[String](
          args(0),
          "6",
          "randomForest",
          getClass.getResource("/savedFeatureMatrices/lex-6.svmLight").getPath,
          "lexical"
        )
      )
    } else {
      println("Your second argument must be '3' or '6'")
    }

  }
}
