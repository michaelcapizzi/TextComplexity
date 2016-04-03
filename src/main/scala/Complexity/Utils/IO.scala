package Complexity.Utils

import java.io.{FileReader, BufferedReader, PrintWriter, File}
import Complexity.{ProcessedParagraph, TextDocument}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.{Document, DocumentSerializer}

/**
  * Contains functions needed to import files and serialize (both `save` and `load`) annotations
  */
object IO {

  /**
    * Imports text from a `.txt` file in resources where '''paragraphs are separated by `\n`'''
 *
    * @param fileName Filename of `.txt` located in [[https://github.com/michaelcapizzi/TextComplexity/tree/master/src/main/resources/rawText]]
    * @return `Vector` of paragraphs in `plain text`
    * @todo Exclude lines between commented out sections
    */
  def importTextFromResources(fileName: String): Vector[String] = {
    //import file
    val is = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/rawText/" + fileName))
    //get lines
    val allLines = is.getLines
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

    is.close()

    finalBuffer += insideBuffer.mkString(" ")           //why do I have to do this one more time here?

    finalBuffer.toVector.                 //convert to vector
      filterNot(_.isEmpty)                //filter out empty
  }


  /**
    * Imports text from any `.txt` file where '''paragraphs are separated by `\n`'''
    *
    * @param fileName Filename of `.txt`
    * @return `Vector` of paragraphs in `plain text`
    * @todo Exclude lines between commented out sections
    */
  def importText(fileName: String): Vector[String] = {
    //getLines
    val allLines = scala.io.Source.fromFile(fileName).getLines

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


  /**
    * Captures author information from metadata of `.txt` file
    *
    * @param fileName Filename of `.txt` located in [[https://github.com/michaelcapizzi/TextComplexity/tree/master/src/main/resources/rawText]]
    * @return Author's name
    */
  def getAuthor(fileName: String): String = {
    val authorRegex = """%(.*)""".r

    val is = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/rawText/" + fileName))

    val line = is.getLines.
                take(1).
                next

    is.close()

    authorRegex.replaceFirstIn(line, """$1""")
  }

  /**
    * Captures title information from metadata of `.txt` file
    *
    * @param fileName Filename of `.txt` located in [[https://github.com/michaelcapizzi/TextComplexity/tree/master/src/main/resources/rawText]]
    * @return Document title
    */
  def getTitle(fileName: String): String = {
    val titleRegex = """%%(.*)""".r

    val is = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/rawText/" + fileName))

    val line = is.
                getLines.
                slice(1,2).
                next

    titleRegex.replaceFirstIn(line, """$1""")
  }

  /**
    * Captures grade level information from metadata of `.txt` file
    *
    * @param fileName Filename of `.txt` located in [[https://github.com/michaelcapizzi/TextComplexity/tree/master/src/main/resources/rawText]]
    * @return Grade level information in `String` format <br>
    *           Because bands of grade levels ('e.g.' K-1 represented as `0001`) will not be properly ordered as `Int`s
    *
    */
  def getGradeLevel(fileName: String): String = {
    val gradeLevelRegex = """^([0-9]+)[A-Z]+_.*""".r
    gradeLevelRegex.replaceFirstIn(fileName, """$1""")
  }


  /**
    * Generates [[ProcessedParagraph]]s from imported plain text
    *
    * @param file Filename of `.txt` located in [[https://github.com/michaelcapizzi/TextComplexity/tree/master/src/main/resources/rawText]]
    * @return   `Vector` of [[ProcessedParagraph]]s, one for each paragraph of original text
    */
  def makeProcParsFromText(file: String): Vector[ProcessedParagraph] = {
    val text = importTextFromResources(file)

    val p = new CoreNLPProcessor(withDiscourse = true, maxSentenceLength = 450)

    for (paragraph <- text) yield {
      new ProcessedParagraph(
        text = Some(paragraph),
        annotatedDoc = None,
        processor = p,
        title = Some(getTitle(file)),
        author = Some(getAuthor(file)),
        gradeLevel = Some(getGradeLevel(file))
      )
    }
  }

  /**
    * Generates [[ProcessedParagraph]]s from serialized `.annotated` file
    *
    * @param annotatedFileName Filename of `.annotated` located in [[https://github.com/michaelcapizzi/TextComplexity/tree/master/src/main/resources/annotatedText]]
    * @return  `Vector` of [[ProcessedParagraph]]s, one for each paragraph of original text
    */
  def makeProcParsFromAnnotation(originalTextFileName: String, annotatedFileName: String): Vector[ProcessedParagraph] = {
    val paragraphs = importSerial(annotatedFileName)

    val p = new CoreNLPProcessor(withDiscourse = true, maxSentenceLength = 450)

    for (par <- paragraphs) yield {
      new ProcessedParagraph(
        text = None,
        annotatedDoc = Some(par),
        processor = p,
        title = Some(getTitle(originalTextFileName)),
        author = Some(getAuthor(originalTextFileName)),
        gradeLevel = Some(getGradeLevel(originalTextFileName))
      )
    }
  }

  /**
    * Use built-in serialize function in `edu.arizona.sista.processors` to save annotations
    *
    * @param annotatedDocuments Vector of '''annotated''' `edu.arizona.sista.processors.Document`s to be saved
    * @param outputFileName Filename to be used in saving annotation to [[https://github.com/michaelcapizzi/TextComplexity/tree/master/src/main/resources/annotatedText]]
    */
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


  /**
    * Use built-in serialize function in `edu.arizona.sista.processors` to load annotations
    *
    * @param annotationFileName Filename of `.annotated` located in [[https://github.com/michaelcapizzi/TextComplexity/tree/master/src/main/resources/annotatedText]]
    * @return `Vector` of `edu.arizona.sista.processors.Document`s to be fed into [[ProcessedParagraph]]s
    */
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

  /**
    * Loads annotations from file and generates a resulting [[TextDocument]]
    *
    * @param annotationFileName Filename of `.annotated` located in [[https://github.com/michaelcapizzi/TextComplexity/tree/master/src/main/resources/annotatedText]]
    * @param processor `edu.arizona.sista.processors.corenlp.CoreNLPProcessor`
    * @return [[TextDocument]] representing the original document.
    */
  def makeDocumentFromSerial(annotationFileName: String, processor: CoreNLPProcessor): TextDocument = {

    //plain text filename
    val textFileName = annotationFileName.dropRight(10) + ".txt"

    //make sista Documents
    val documentVector = importSerial(annotationFileName)

    //get metadata from original file
    val author = getAuthor(textFileName)
    val title = getTitle(textFileName)
    val gradeLevel = getGradeLevel(textFileName)

    //make ProcessedParagraphs
    val procPars = for (doc <- documentVector) yield {
                    new ProcessedParagraph(
                                            text = None,
                                            annotatedDoc = Some(doc),
                                            processor = processor,
                                            title = Some(title),
                                            author = Some(author),
                                            gradeLevel = Some(gradeLevel)
                                          )
                    }

    //make TextDocument
    new TextDocument(
                      paragraphs = procPars,
                      title = Some(title),
                      author = Some(author),
                      gradeLevel = Some(gradeLevel)
                    )

  }

}
