package Complexity

import java.io.{PrintWriter, File}

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

      //get the matrix file
      val buffered = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/savedFeatureMatrices/lex_par-3.svmLight")).getLines
      //write it to local file
      val localFile = new File("./matrix.svmLight")
      val pw = new PrintWriter(localFile.getCanonicalPath)
      buffered.foreach(pw.println)
      pw.close()


      //calls Predict Main class but uses best parameters: svm, lexical and paragraph features
      Predict.main(
        Array[String](
          args(0),
          "3",
          "svm",
          localFile.getCanonicalPath,
          "lexical",
          "paragraph"
        )
      )

    } else if (args(1) == "6") {

      val buffered = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/savedFeatureMatrices/lex-6.svmLight")).getLines
      //write it to local file
      val localFile = new File("./matrix.svmLight")
      val pw = new PrintWriter(localFile.getCanonicalPath)
      buffered.foreach(pw.println)
      pw.close()

      //calls Predict Main class but uses best parameters: random forest, lexical features
      Predict.main(
        Array[String](
          args(0),
          "6",
          "randomForest",
          localFile.getCanonicalPath,
          "lexical"
        )
      )

    } else {
      println("Your second argument must be '3' or '6'")
    }


  }
}
