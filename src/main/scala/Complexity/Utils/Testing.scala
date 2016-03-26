package Complexity.Utils

import Complexity.ProcessedParagraph
import Complexity.Utils.IO._
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

/**
  * Created by mcapizzi on 3/25/16.
  */
object Testing {

  //files to use for tests
  val owlFile = "0001AL_OwlAndMoon.txt"

  //create processor
  val p = new CoreNLPProcessor(withDiscourse = true)

  def makeProcPars(file: String): Vector[ProcessedParagraph] = {
    val text = importText(file)

    for (paragraph <- text) yield {
      new ProcessedParagraph(
                              text = paragraph,
                              processor = p,
                              author = Some(getAuthor(file)),
                              gradeLevel = Some(getGradeLevel(file))
      )
    }
  }

}
