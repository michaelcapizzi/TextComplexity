package Complexity.SupportMethods


/**
 * Support variables and methods for use in [[Complexity.Features.LexicalFeatures.getWordConcreteness]]
 */
object Concreteness {

  /**
    * Raw version of the concreteness data `.csv` file
    */
  val concretenessRaw = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/concretenessData.csv")).getLines.toVector.map(_.split(","))

  /**
    * Concreteness data converted to a parallelized `Map`
    */
  val concretenessMap = concretenessRaw.
                            drop(1).                    //drops the headers
                            map(item =>
                                item(0) ->              //the word
                                item(2).toDouble        //it's concreteness score
                        ).toMap.par                     //convert to parallel map

}
