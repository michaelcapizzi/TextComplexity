package Complexity.SupportMethods

/**
  * Support methods for calculating coherence
  */
object Coherence {

  //TODO consider building alternative method (not including set intersection) that will handle pronouns and w2v more easily

  //finds intersection of a sequence of sets
  //used for plain noun matching
  //TODO fix: capturing pronouns as determiners when they appear in two consecutive sentences EVEN IF they aren't coreferent
  def multiIntersect(listOfSets: Vector[Set[String]]): Set[String] = {

    def loop(listOfSets: Vector[Set[String]], runningIntersection: Set[String]): Set[String] = {
      if (listOfSets.tail.isEmpty) {
        val intersection = runningIntersection.intersect(listOfSets.head)
        intersection
      } else {
        val intersection = runningIntersection.intersect(listOfSets.head)
        loop(listOfSets.tail, intersection)
      }
    }

    loop(listOfSets, listOfSets.head)
  }

  //finds pronouns in nsubj position to add to coherenceGrid
  def findPronouns(listOfSets: Vector[Set[(String, String)]]): Set[String] = {
    val pronounRegex = """(this)|(that)|(he)|(she)|(they)|(it)"""

    //if pronoun in any set in the window OTHER THAN FIRST SET add it to output
    (for (sentence <- listOfSets.drop(1)) yield {
      sentence.filter(tuple =>
        tuple._1.matches(pronounRegex) &&               //matches pronounRegex
          tuple._2.matches("""(nsubj)|(nsubjpass)""")     //is in nsubj or nsubjpass position
      ).map(_._1)                                       //keep just the word
    }).flatten.toSet                                    //flatten and turn to set
  }


}
