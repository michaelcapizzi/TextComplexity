package Complexity.MachineLearning

import java.io.File

/**
  * Runs all experiments on all datasets
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
      val numClasses = regex.findFirstIn(f.getName)

      println(f.getName)
      RunExperiments.main(Array(
                                f.getPath,
                                numClasses.get,
                                "all"
                                )
      )

    }
  }
}
