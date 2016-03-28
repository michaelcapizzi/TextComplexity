import Complexity.Features.{SyntacticFeatures, LexicalFeatures}
import Complexity.TextDocument
import Complexity.Utils.IO._
import Complexity.Utils.Testing._
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import org.scalatest._

/**
  * Created by mcapizzi on 3/25/16.
  */

abstract class GeneralTest extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors{

  //file to use for test
  val file = "0001AL_OwlAndMoon.txt"
  //  val annotation = "0000Test.annotated"
  val annotation = "0001AL_OwlAndMoon.annotated"

  //create processor
  val p = new CoreNLPProcessor(withDiscourse = true)

  //make ProcessedParagraphs
  val procParsFromText = makeProcParsFromText(file)

  //annotate
  procParsFromText.foreach(_.annotate)

  //import annotation
  val procParsFromAnnotation = makeProcParsFromAnnotation(file, annotation)

  //make TextDocuments
  val tdFromText = new TextDocument(procParsFromText)
  val tdFromAnnotation = makeDocumentFromSerial(annotation, p)

  //features
  val lex = new LexicalFeatures(tdFromText)
  val syn = new SyntacticFeatures(tdFromText)

}

//write specific tests with class X extends GeneralTest

/*  example
class SampleTest extends GeneralTest {

  val (s, _, _, _, _, _) = demoJSON("GF6", p, pF, false)

  "A SpeechDoc" should "have a first sentence of 'mice are left hemisphere thinks linearly and methodically are left .' " in {

    assert(s.words(false).head == Vector("mice", "are", "left", "hemisphere", "thinks", "linearly", "and", "methodically", "are", "left"))

  }

}
*/
