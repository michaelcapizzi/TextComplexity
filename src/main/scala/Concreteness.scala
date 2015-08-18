import scala.io.Source

/**
 * Created by mcapizzi on 8/17/15.
 */
object Concreteness {

  val concretenessRaw = Source.fromURL("https://s3-us-west-2.amazonaws.com/capizzi-nlp/Concreteness/SISTA_concreteness.csv").getLines.toVector.map(_.split(","))

  val concretenessMap = concretenessRaw.map(item =>
    item(0) ->                                        //the word
    item(2)                                           //it's concreteness score
  ).drop(1).                                          //drops the headers
    toMap.par                                         //make a parallel collection hashMap

}
