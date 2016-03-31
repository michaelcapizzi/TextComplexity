package Complexity.SupportMethods

/**
  * Support methods for decomposing `DirectedGraph` of [[Complexity.TextDocument.rawDependencies]]
  */
object Dependencies {

  /**
    * Converts `DirectedGraph` to a tuple
 *
    * @param deps [[Complexity.TextDocument.rawDependencies]]
    * @return `(word index, relation index, dependency)`
    */
  def cleanDep(deps: Array[edu.arizona.sista.struct.DirectedGraph[String]]): Vector[Vector[(Int, Int, String)]] = {
    //buffer to house clean dependencies
    val buffer = scala.collection.mutable.Buffer[Vector[(Int, Int, String)]]()

    //iterate through all sentences
    for (sentence <- deps) yield {
      //buffer to house dependencies as they are cleaned
      val sentenceBuffer = scala.collection.mutable.Buffer[(Int, Int, String)]()

      //for each edge
      for (edge1 <- 0 to sentence.size - 1) yield {
        if (sentence.getOutgoingEdges(edge1).nonEmpty) {
          for (edge2 <- sentence.getOutgoingEdges(edge1)) {
            sentenceBuffer +=
              (
                (
                  edge1,        //word index
                  edge2._1,     //relation index
                  edge2._2      //dependency
                  )
                )
          }
        }
      }
      buffer += sentenceBuffer.toVector.distinct
    }
    buffer.toVector
  }

}
