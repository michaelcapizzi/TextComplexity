package Complexity.Utils

import java.io.File
import Complexity.Utils.Testing._
import Complexity.Utils.IO._


object AnnotateTexts {

  /**
    * Generates serialized annotations for all files in a directory and saves to resources
    * @param args (0) directory containing all plain text files
    */
  def main(args: Array[String]) = {

    //get list of files to annotate
    val allFiles = new File(args(0)).listFiles

    //get list of finished annotations
    val finishedFiles = new File(getClass.getResource("/annotatedText/").getPath).listFiles
    //get list of finished names
    val finishedNames = finishedFiles.map(_.getName).toSet

    //iterate through files
    for (f <- allFiles.filterNot(_.getName.endsWith("Quixote.txt"))) {

      //variables for naming
      val fullName = f.getName
      val annotatedName = fullName.dropRight(4) + ".annotated"

      if (finishedNames.contains(annotatedName)) {

        println("already annotated " + fullName)

      } else {

        println("annotating " + fullName)

        //make processed paragraphs
        val procPars = makeProcParsFromText(fullName)

        //annotate all paragraphs
        procPars.foreach(_.annotate)

        //extract SISTA documents
        val docs = procPars.map(_.doc)

        //serialize
        serializeAnnotation(docs, annotatedName)
      }
    }
  }

}
