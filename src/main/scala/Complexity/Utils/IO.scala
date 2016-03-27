package Complexity.Utils

import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

/**
  * Created by mcapizzi on 3/25/16.
  */
object IO {

  //imports text into Vector of paragraphs
    //text has a blank line between paragraphs
  //TODO fromInputStream to allow from inside .jar
  def importText(fileName: String): Vector[String] = {
    //import file
    val allLines = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/rawText/" + fileName)).getLines
    //buffer to hold paragraphs
    val finalBuffer = collection.mutable.Buffer[String]()
    //intermediate buffer
    val insideBuffer = collection.mutable.Buffer[String]()

    for (line <- allLines) {
      if (!line.startsWith("%")) {                      //skip metadata lines
        if (!line.isEmpty && line != null) {            //while line isn't empty or null
          insideBuffer += line                              //add to insideBuffer
        } else if (line.isEmpty || line == null) {      //when line is empty or null
          finalBuffer += insideBuffer.mkString(" ")         //shift to finalBuffer
          insideBuffer.clear                                //clear insideBuffer
        }
      }
    }
    finalBuffer += insideBuffer.mkString(" ")           //why do I have to do this one more time here?

    finalBuffer.toVector.                 //convert to vector
      filterNot(_.isEmpty)                //filter out empty
  }

  //get author from metadata
  def getAuthor(filePath: String): String = {
    val authorRegex = """%(.*)""".r
    val line = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/rawText/" + filePath)).
                getLines.
                take(1).
                next
    authorRegex.replaceFirstIn(line, """$1""")
  }

  //get title from metadata
  def getTitle(filePath: String): String = {
    val titleRegex = """%%(.*)""".r
    val line = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/rawText/" + filePath)).
      getLines.
      slice(1,2).
      next
    titleRegex.replaceFirstIn(line, """$1""")
  }

  //get grade level from filename
  def getGradeLevel(filePath: String): String = {
    val gradeLevelRegex = """.*\/([0-9]+)[A-Z]+_.*""".r
    gradeLevelRegex.replaceFirstIn(filePath, """$1""")
  }


}
