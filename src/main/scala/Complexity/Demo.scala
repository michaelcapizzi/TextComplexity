package Complexity

/**
  * Uses best model
  */
object Demo {

  /**
    *
    * @param args args(0) file to test
    */
  def main(args: Array[String]) = {

    //calls Predict Main class but uses best parameters
    Predict.main(
                  Array[String](
                                  args(0),
                                  "3",
                                  "lexical"
                  )
    )
  }
}
