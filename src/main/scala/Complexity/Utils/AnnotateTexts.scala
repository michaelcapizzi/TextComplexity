package Complexity.Utils

import java.io.File
import Complexity.Utils.Testing._
import Complexity.Utils.IO._


/**
  * Created by mcapizzi on 3/27/16.
  */
object AnnotateTexts {

  /**
    * Generates serialized annotations for all files in a directory and saves to resources
    * args(0) = directory containing all plain text files
    */

  def main(args: Array[String]) = {

    //get list of files to annotate
    val allFiles = new File(args(0)).listFiles

    //iterate through files
    for (f <- allFiles) {

      val fullName = f.getName
      val annotatedName = fullName.dropRight(4) + ".annotated"

      //make procesed paragraphs
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
