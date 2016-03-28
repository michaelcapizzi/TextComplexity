package Complexity.Utils

import java.io.{FileReader, BufferedReader, PrintWriter, File}
import Complexity.TextDocument
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.{Document, DocumentSerializer}

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

  //serialize annotation to file
  def serializeAnnotation(annotatedDocuments: Vector[Document], outputFileName: String): Unit = {
    val writeToFile = new PrintWriter(
                        new File(
                          "/media/mcapizzi/data/Github/TextComplexity/src/main/resources/annotatedText/" + outputFileName
//                          getClass.getResource("/annotatedText/" + outputFileName).getPath  //TODO how to write to resources without absolute path?
                        )
                      )

    val serializer = new DocumentSerializer

    //serialize each paragraph's annotations
    annotatedDocuments.foreach(serializer.save(_, writeToFile))

    writeToFile.close
  }

  //import serialized annotation into vector of annotated paragraphs
  def importSerial(annotationFileName: String): Vector[Document] = {
    //serializer
    val serial = new DocumentSerializer
    //file
    val f = new File(getClass.getResource("/annotatedText/" + annotationFileName).getPath)
    //open buffer
    val buffer = new BufferedReader(new FileReader(f))

    //read in first line
    var line = buffer.readLine
    //initialize other variables to be used
    val stringBuffer = new StringBuilder
    val docBuffer = collection.mutable.Buffer[String]()

    while (line != null) {
      if (line == "EOD") {                    //if end of annotation of paragraph
        stringBuffer.append(line)
        docBuffer += stringBuffer.toString
        stringBuffer.clear
        line = buffer.readLine
      } else {
        stringBuffer.append(line + "\n")
        line = buffer.readLine
      }
    }
    //for each paragraph in docBuffer
    docBuffer.map(paragraph =>
                                serial.load(paragraph)
    ).toVector
  }

  //import directly from annotation and make text document
  /*def makeDocumentFromSerial(annotationFileName: String, processor: CoreNLPProcessor): TextDocument = {
    //make sista Documents
    val documentVector = importSerial(annotationFileName)

    val text = document.map(_.sentences.map(_.words.toVector).flatten.mkString(" "))
    val author = getAuthor(fullOriginalFilePath)
    val title = getTitleChapter(fullOriginalFilePath)._1
    val chapter = getTitleChapter(fullOriginalFilePath)._2
    val gradeLevel = getGradeLevel(fullOriginalFilePath)
    new TextDocument(text, processor, document, author, title, chapter, gradeLevel)
  }*/

}
