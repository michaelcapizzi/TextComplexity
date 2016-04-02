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
    */
  def main(args: Array[String]) = {

    //calls Predict Main class but uses best parameters
    Predict.main(
                  Array[String](
                                  args(0),
                                  "3",
                                  getClass.getResource("/savedFeatureMatrices/lex-3.svmLight").getPath,
                                  "lexical"
                  )
    )
  }
}
