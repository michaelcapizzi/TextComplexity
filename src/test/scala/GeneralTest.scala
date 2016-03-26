import org.scalatest._

/**
  * Created by mcapizzi on 3/25/16.
  */

abstract class GeneralTest extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors{

  //write specific tests with class X extends GeneralTest

}

/*  example
class SampleTest extends GeneralTest {

  val (s, _, _, _, _, _) = demoJSON("GF6", p, pF, false)

  "A SpeechDoc" should "have a first sentence of 'mice are left hemisphere thinks linearly and methodically are left .' " in {

    assert(s.words(false).head == Vector("mice", "are", "left", "hemisphere", "thinks", "linearly", "and", "methodically", "are", "left"))

  }

}
*/
