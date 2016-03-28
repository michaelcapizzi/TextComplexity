package Complexity.SupportMethods


/**
 * Created by mcapizzi on 8/17/15.
 */
object Concreteness {

  val concretenessRaw = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/concretenessData.csv")).getLines.toVector.map(_.split(","))

  val concretenessMap = concretenessRaw.
                            drop(1).                    //drops the headers
                            map(item =>
                                item(0) ->              //the word
                                item(2).toDouble        //it's concreteness score
                        ).toMap.par                     //convert to parallel map

}
