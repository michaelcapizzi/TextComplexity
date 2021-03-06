package Complexity.SupportMethods

import scala.collection.mutable
import scala.io.Source

/**
 * Old method for importing `.txt` into paragraphs
 */
object Importing {

  def importParagraphsFromFile(filePath: String): Vector[String] = {
    val finalBuffer = mutable.Buffer[String]()
    val insideBuffer = mutable.Buffer[String]()

    for (line <- Source.fromFile(filePath).getLines.filter(_.startsWith("%"))) {
      if (!line.isEmpty && line != null) insideBuffer += line             //at each blank line, it starts a new "List" to indicate a new paragraph
      else if (line.isEmpty || line == null) {
        finalBuffer += insideBuffer.mkString(" ")                         //add last paragraph
        insideBuffer.clear                                                //return full document
      }
    }
    finalBuffer += insideBuffer.mkString(" ")
    finalBuffer.toVector.filterNot(_.isEmpty)
  }





}
