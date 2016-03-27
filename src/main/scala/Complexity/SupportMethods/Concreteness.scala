package Complexity.SupportMethods


/**
 * Created by mcapizzi on 8/17/15.
 */
object Concreteness {

  val concretenessRaw = scala.io.Source.fromInputStream(getClass.getResourceAsStream("concretenessData.csv")).getLines.toVector.map(_.split(","))

  val concretenessMap = concretenessRaw.map(item =>
    item(0) ->                                        //the word
    item(2).toDouble                                  //it's concreteness score
  ).drop(1).                                          //drops the headers
    toMap.par                                         //make a parallel collection hashMap

}
