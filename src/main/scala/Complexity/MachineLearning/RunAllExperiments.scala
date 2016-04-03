package Complexity.MachineLearning

import java.io.File

/**
  * Runs all experiments on all datasets
  * @todo How to suppress logger to capture scores
  */
object RunAllExperiments {

  def main(args: Array[String]) = {
    //regex to capture number of classes from dataset filenames
    val regex = """.*([0-9]).*""".r

    //location of all datasets
    val datasetFolder = new File(getClass.getResource("/savedFeatureMatrices").getPath)

    val allDatasets = datasetFolder.listFiles

    for (f <- allDatasets) {
      //extract number of classes from dataset file
      val numClasses = f.getName match {
                                        case regex(n) => n
                                        }


      RunExperiments.main(Array(
                                f.getPath,
                                numClasses,
                                "all"
                                )
      )
      println(f.getName)
    }
  }
}
