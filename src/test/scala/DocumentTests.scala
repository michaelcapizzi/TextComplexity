import Complexity.ProcessedParagraph
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import Complexity.Utils.IO._

/**
  * Created by mcapizzi on 3/25/16.
  */
class DocumentTests extends GeneralTest {

  //file to use for test
  val file = "0001AL_OwlAndMoon.txt"

  //import text
  val text = importText(file)

  //create processor
  val p = new CoreNLPProcessor(withDiscourse = true)

  //make ProcessedParagraphs
  val procPars = for (paragraph <- text) yield {
                  new ProcessedParagraph(
                                          text = paragraph,
                                          processor = this.p,
                                          author = Some(getAuthor(file)),
                                          gradeLevel = Some(getGradeLevel(file))
                  )
  }

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
    val allEntities = procPars.flatMap(_.getProperNouns).flatten
    assert(allEntities.length == 2 && allEntities.contains("Owl") && allEntities.contains("Moon"))
  }



}

