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

}
