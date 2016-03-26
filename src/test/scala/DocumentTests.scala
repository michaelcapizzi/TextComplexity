import Complexity.ProcessedParagraph
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import Complexity.Utils.IO._
import Complexity.Utils.Testing._

/**
  * Created by mcapizzi on 3/25/16.
  */
class DocumentTests extends GeneralTest {

  //file to use for test
  val file = "0001AL_OwlAndMoon.txt"

  //create processor
  val p = new CoreNLPProcessor(withDiscourse = true)

  //make ProcessedParagraphs
  val procPars = makeProcPars(file)

  //annotate
  procPars.foreach(_.annotate)


  ///////tests///////

  "Words without punctuation" should "be shorter than words with punctuation" in {
    for (paragraph <- procPars) {
      val withPunct = paragraph.words(withPunctuation = true).flatten
      val withoutPunct = paragraph.words(withPunctuation = false).flatten
      assert(withPunct.length > withoutPunct.length)
    }
  }

  "The proper nouns" should "be Owl and Moon" in {
    val allEntities = procPars.flatMap(_.getProperNouns).distinct
    assert(allEntities.length == 2)
  }



}

