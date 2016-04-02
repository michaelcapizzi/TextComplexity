package Complexity

import MachineLearning.MLutils._

/**
  * Uses best model
  */
object Demo {

  //TODO figure out how to load model!
  /**
    *
    * @param args args(0) = file to test
    *             args(1) = number of classes `3` or `6`
    */
  def main(args: Array[String]) = {

    if (args(1) == "3") {
      //calls Predict Main class but uses best parameters
      Predict.main(
        Array[String](
          args(0),
          args(1),
          getClass.getResource("/savedFeatureMatrices/lex-3.svmLight").getPath,
          "lexical"
        )
      )
    } else if (args(1) == "6") {
      //calls Predict Main class but uses best parameters
      Predict.main(
        Array[String](
          args(0),
          args(1),
          getClass.getResource("/savedFeatureMatrices/lex-6.svmLight").getPath,
          "lexical"
        )
      )
    } else {
      println("Your second argument must be '3' or '6'")
    }

  }
}
