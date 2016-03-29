package Complexity.Utils

import Complexity.ProcessedParagraph
import Complexity.Utils.IO._
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

/**
  * Object containing methods and variables to be used in internal testing
  */
object Testing {

  //files to use for tests
  val owlFile = "0001AL_OwlAndMoon.txt"

  //create processor
  val p = new CoreNLPProcessor(withDiscourse = true, maxSentenceLength = 450)

  //generate ProcessedParagraphs from plain text file
  def makeProcParsFromText(file: String): Vector[ProcessedParagraph] = {
    val text = importText(file)

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

  //generate ProcessedParagraphs from annotation
  def makeProcParsFromAnnotation(originalTextFileName: String, annotatedFileName: String): Vector[ProcessedParagraph] = {
    val paragraphs = importSerial(annotatedFileName)

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

}
