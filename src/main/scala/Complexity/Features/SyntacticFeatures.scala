package Complexity.Features

import java.util
import Complexity.TextDocument
import edu.arizona.sista.struct.Counter
import edu.stanford.nlp.trees.tregex.TregexPattern
import edu.stanford.nlp.trees.CollinsHeadFinder
import org.apache.commons.math3.stat.Frequency
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
import scala.collection.JavaConverters._



/**
 * Generates features syntactic in nature (at the sentence level)
  * @param td  [[TextDocument]] for the document to be analyzed
 */
class SyntacticFeatures(val td: TextDocument) {

  /**
    * Calculates sentence lengths '''not including punctuation'''
    * @return `Vector` of sentence lengths
    */
  def getSentenceLengths: Vector[Double] = {
    val allSentences = this.td.words(withPunctuation = false).flatten  //remove paragraphs

    allSentences.map(_.length.toDouble)
  }


  /**
    * Generates basic distribution of sentences lengths
     * @return Values representing distribution of sentence lengths
    *
    *         {{{
    *           Map(
    *             "sentence length minimum" -> ?,
    *             "25th %ile sentence length" -> ?,
    *             "sentence length mean" -> ?,
    *             "sentence length median" -> ?,
    *             "75th %ile sentence length" -> ?,
    *             "sentence length maximum" -> ?
    *           )
    *         }}}
    */
  def sentenceLengthStats: Map[String, Double] = {
    //call descriptive stats
    val stat = new DescriptiveStatistics()
    //add to stats
    this.getSentenceLengths.foreach(stat.addValue)

    Map(
      "sentence length minimum" -> stat.getMin,
      "25th %ile sentence length" -> stat.getPercentile(50),
      "sentence length mean" -> stat.getMean,
      "sentence length median" -> stat.getPercentile(50),
      "75th %ile sentence length" -> stat.getPercentile(75),
      "sentence length maximum" -> stat.getMax
    )
  }


  /**
    * Finds all punctuation in each sentence
     * @return `Vector` of only punctuation
    */
  def getPunctuation: Vector[Vector[String]] = {
    val sentences = td.words(withPunctuation = true).flatten    //remove paragraphs

    //filter to only keep punctuation
    for (sent <- sentences) yield {
      sent.filterNot(token => token.matches("[A-z0-9]+"))
    }

  }


  /**
    * Generates basic distribution of surplus punctuation <br>
    *   "surplus punctuation" = anything more than an end-of-sentence marker
     * @return Values representing distribution of surplus punctuation
    *
    *         {{{
    *           Map(
    *             "25th %ile surplus punctuation size" -> ?,
    *             "mean surplus punctuation size" -> ?,
    *             "median surplus punctuation size" -> ?,
    *             "75th %ile surplus punctuation size" -> ?,
    *             "maximum surplus punctuation size" -> ?
    *           )
    *         }}}
    */
  //"surplus" punctuation = anything more than an end-of-sentence marker
  def punctuationStats: Map[String, Double] = {
    //call descriptive stats
    val stat = new DescriptiveStatistics()
    //surplus punctuation counts
    val surplusCounts = for (sentence <- this.getPunctuation) yield {
                          sentence.dropRight(1).        //drop end-of-sentence marker
                          length.toDouble
    }
    //count of sentences with surplus punctuation
    val surplusNumber = surplusCounts.count(_ != 0d).toDouble
    //total number of sentence
    val totalSentenceNumber = surplusCounts.length.toDouble

    //add to stats
    surplusCounts.foreach(stat.addValue)

    Map(
      "percent of sentences with surplus punctuation" -> surplusNumber / totalSentenceNumber,
      "25th %ile surplus punctuation size" -> stat.getPercentile(25),
      "mean surplus punctuation size" -> stat.getMean,
      "median surplus punctuation size" -> stat.getPercentile(50),
      "75th %ile surplus punctuation size" -> stat.getPercentile(75),
      "maximum surplus punctuation size" -> stat.getMax
    )
  }


  /**
    * Finds size of each parse tree
    * "size" of parse tree = total number of nodes
     * @return `Vector` of parse tree sizes
    */
  def getTreeSizes: Vector[Double] = {
    td.coreNLPParseTrees.
      flatten.              //remove paragraphs
      map(_.size.toDouble)
  }


  /**
    * Generates basic distribution of parse tree sizes
     * @return Values representing distribution of parse tree sizes
    *
    *         {{{
    *           Map(
    *             "minimum tree size" -> ?,
    *             "25th %ile tree size" -> ?,
    *             "mean tree size" -> ?,
    *             "median tree size" -> ?,
    *             "75th %ile tree size" -> ?,
    *             "maximum tree size" -> ?
    *           )
    *         }}}
    */
  def treeSizeStats: Map[String, Double] = {
    //call descriptive stats
    val stat = new DescriptiveStatistics()
    //add to stats
    this.getTreeSizes.foreach(stat.addValue)

    Map(
      "minimum tree size" -> stat.getMin,
      "25th %ile tree size" -> stat.getPercentile(25),
      "mean tree size" -> stat.getMean,
      "median tree size" -> stat.getPercentile(50),
      "75th %ile tree size" -> stat.getPercentile(75),
      "maximum tree size" -> stat.getMax
    )
  }


  /**
    * Finds the depth of each parse tree <br>
    * "depth" of parse tree = number of layers in tree
     * @return `Vector` of parse tree depths
    */
  def getTreeDepths: Vector[Double] = {
    td.coreNLPParseTrees.
      flatten.                //remove paragraphs
      map(_.depth.toDouble)
  }


  /**
    * Generates basic distribution of parse tree depths
     * @return Values representing distribution of parse tree depths
    *
    *         {{{
    *           Map(
    *             "minimum tree depth" -> ?,
    *             "25th %ile tree depth" -> ?,
    *             "mean tree depth" -> ?,
    *             "median tree depth" -> ?,
    *             "75th %ile tree depth" -> ?,
    *             "maximum tree depth" -> ?
    *           )
    *         }}}
    *
    */
  def treeDepthStats: Map[String, Double] = {
    //call descriptive stats
    val stat = new DescriptiveStatistics()
    //add to stats
    this.getTreeDepths.foreach(stat.addValue)

    Map(
      "minimum tree depth" -> stat.getMin,
      "25th %ile tree depth" -> stat.getPercentile(25),
      "mean tree depth" -> stat.getMean,
      "median tree depth" -> stat.getPercentile(50),
      "75th %ile tree depth" -> stat.getPercentile(75),
      "maximum tree depth" -> stat.getMax
    )
  }


  /**
    * Finds the distance to main verb for each sentence
    * "distance to verb" calculated as the index of the root of the parse tree as identified by `edu.stanford.nlp.trees.CollinsHeadFinder`
    * @see [[http://nlp.stanford.edu/nlp/javadoc/javanlp-3.5.0/edu/stanford/nlp/trees/CollinsHeadFinder.html]]
    * @return `Vector` of distances to verb
    */

  def getDistanceToVerb: Vector[Double] = {
    //initiate head finder
    val cHF = new CollinsHeadFinder()

    //all trees
    val trees = td.coreNLPParseTrees.flatten      //removes paragraphs
    for (t <- trees) yield {
      val head = t.headTerminal(cHF).label().toString     //returns word-index
      val index = head.split("-").last.toDouble           //extract just index
      index
    }
  }


  /**
    * Generates basic distribution of distances to verbs
     * @return Values representing distribution of distances to verb
    *
    *         {{{
    *           Map(
    *             "minimum distance to verb" -> ?,
    *             "25th %ile distance to verb" -> ?,
    *             "mean distance to verb" -> ?,
    *             "median distance to verb" -> ?,
    *             "75th %ile distance to verb" -> ?,
    *             "maximum distance to verb" -> ?
    *           )
    *         }}}
    */
  def distanceToVerbStats: Map[String, Double] = {
    //call descriptive stats
    val stat = new DescriptiveStatistics()
    //add to stats
    this.getDistanceToVerb.foreach(stat.addValue)

    Map(
      "minimum distance to verb" -> stat.getMin,
      "25th %ile distance to verb" -> stat.getPercentile(25),
      "mean distance to verb" -> stat.getMean,
      "median distance to verb" -> stat.getPercentile(50),
      "75th %ile distance to verb" -> stat.getPercentile(75),
      "maximum distance to verb" -> stat.getMax
    )
  }


  /**
    * Finds the number of constituents for each sentence
    * @return `Vector` of count of constituents
    */
  def getConstituentCounts: Vector[Double] = {
    td.rawConstituents.
      flatten.            //remove paragraphs
      map(con =>          //for each constituent
      con.size.toDouble)  //get its size
  }


  /**
    * Generates basic distribution of constituent counts
     * @return Values representing distibution of constituent counts
    *
    *         {{{
    *           Map(
    *             "minimum number of constituents in a sentence" -> ?,
    *             "25th %ile number of constituents in a sentence" -> ?,
    *             "mean number of constituents in a sentence" -> ?,
    *             "median number of constituents in a sentence" -> ?,
    *             "75th %ile number of constituents in a sentence" -> ?,
    *             "maximum number of constituents in a sentence" -> ?
    *           )
    *         }}}
    */
  def constituentCountStats: Map[String, Double] = {
    //call descriptive stats
    val stat = new DescriptiveStatistics()
    //add to stats
    this.getConstituentCounts.foreach(stat.addValue)

    Map(
      "minimum number of constituents in a sentence" -> stat.getMin,
      "25th %ile number of constituents in a sentence" -> stat.getPercentile(25),
      "mean number of constituents in a sentence" -> stat.getMean,
      "median number of constituents in a sentence" -> stat.getPercentile(50),
      "75th %ile number of constituents in a sentence" -> stat.getPercentile(75),
      "maximum number of constituents in a sentence" -> stat.getMax
    )
  }


  /**
    * Finds the number of words in each constituent
    * @return `Vector` of number of constituent sizes
    */
  def getConstituentSizes: Vector[Double] = {
    //convert constituents to Scala vectors
    val consScala = td.rawConstituents.
                      flatten.                  //remove paragraphs
                      flatMap(con =>
                        con.asScala.toVector)   //cast as Scala

    //get size of constituent
    consScala.map(_.size.toDouble)

  }


  /**
    * Generates as basic distribution of consituent sizes
     * @return Values representing distribution of constituent sizes
    *
    *         {{{
    *           Map(
    *             "constituent length minimum" -> ?,
    *             "25th %ile constituent length" -> ?,
    *             "constituent length mean" -> ?,
    *             "constituent length median" -> ?,
    *             "75th %ile constituent length" -> ?,
    *             "constituent length maximum" -> ?
    *           )
    *         }}}
    */
  def constituentSizeStats: Map[String, Double] = {
    //call descriptive stats
    val stat = new DescriptiveStatistics()
    //add to stats
    this.getConstituentSizes.foreach(stat.addValue)

    Map(
      "constituent length minimum" -> stat.getMin,
      "25th %ile constituent length" -> stat.getPercentile(25),
      "constituent length mean" -> stat.getMean,
      "constituent length median" -> stat.getPercentile(50),
      "75th %ile constituent length" -> stat.getPercentile(75),
      "constituent length maximum" -> stat.getMax
    )
  }


  /**
    * pattern generated for detecting clauses using `edu.stanford.nlp.trees.tregex`
    * @see [[http://nlp.stanford.edu/manning/courses/ling289/Tregex.html]]
    * @see [[http://personal.psu.edu/xxl13/papers/Lu_inpress_ijcl.pdf]]
    */
  val clause = TregexPattern.compile("S [< (VP < (VP . CC <# MD|VBD|VBP|VBZ)) | < (VP <# MD|VBD|VBP|VBZ)]")
  //matches any S node that either (1) dominates a VP whose head is a finite verb or (2) dominates a VP consisting of conjoined VPs whose head is a finite verb

  /**
    * pattern generated for detecting fragments using `edu.stanford.nlp.trees.tregex`
    * @see [[http://nlp.stanford.edu/manning/courses/ling289/Tregex.html]]
    * @see [[http://personal.psu.edu/xxl13/papers/Lu_inpress_ijcl.pdf]]
    */
  val fragment = TregexPattern.compile("ROOT !<< VP")
  //matches any tree without a VP

  /**
    * pattern generated for detecting independent clauses using `edu.stanford.nlp.trees.tregex`
    * @see [[http://nlp.stanford.edu/manning/courses/ling289/Tregex.html]]
    * @see [[http://personal.psu.edu/xxl13/papers/Lu_inpress_ijcl.pdf]]
    */
  val independentClause = TregexPattern.compile("S !> SBAR [< (VP < (VP . CC <# MD|VBD|VBP|VBZ)) | < (VP <# MD|VBD|VBP|VBZ)]")
  //matches any S node that is a clause but is NOT dominated by an SBAR

  /**
    * pattern generated for detecting dependent clauses using `edu.stanford.nlp.trees.tregex`
    * @see [[http://nlp.stanford.edu/manning/courses/ling289/Tregex.html]]
    * @see [[http://personal.psu.edu/xxl13/papers/Lu_inpress_ijcl.pdf]]
    */
  val dependentClause = TregexPattern.compile("S > SBAR [< (VP < (VP . CC <# MD|VBD|VBP|VBZ)) | < (VP <# MD|VBD|VBP|VBZ)]")
  //matches any S node that is a clause that IS dominated by an SBAR


  /**
    * Finds total number of clauses per sentence
     * @return `Vector` of number of clauses
    */
  def getClauseCounts: Vector[Double] = {
    val allTrees = td.coreNLPParseTrees.flatten
    for (tree <- allTrees) yield {
      var counter = 0
      //use Tregex matcher
      val clauseMatcher = clause.matcher(tree)
      while (clauseMatcher.find) {
        counter += 1
      }
      counter.toDouble
    }
  }


  /**
    * Generates feature representing sentence complexity <br>
    *   sentence complexity = total number of clauses / total number of sentences <br>
    * @example A document with all simple sentences would have a `sentence complexity` of 1.
    * @return Value representing sentence complexity
    */
  //# of clauses / # of sentences
  def getSentenceComplexityScore: Double = {
    this.getClauseCounts.sum / td.totalSentences
  }


  /**
    * Finds size and depth of each clause's parse tree
    * @ see [[getTreeSizes]]
    * @return `Vector` of clause parse tree sizes and depths
    */
  //the (size, depth) of all clauses
  def getClauseSizesDepths: Vector[Map[String, Double]] = {
    val allTrees = td.coreNLPParseTrees.flatten

    (for (tree <- allTrees) yield {
      //use Tregex matcher
      val clauseMatcher = this.clause.matcher(tree)
      //initialize buffer to hold results
      val counterBuffer = collection.mutable.Buffer[Map[String, Double]]()
      //iterate through clauses
      while (clauseMatcher.find) {
        val size = clauseMatcher.getMatch.size.toDouble
        val depth = clauseMatcher.getMatch.depth.toDouble
        counterBuffer += Map("size" -> size, "depth" -> depth)
      }
      counterBuffer.toVector
    }).flatten

  }


  /**
    * Generates basic distribution of clause parse tree sizes
     * @return Values representing distribution of clause parse tree sizes
    *
    *         {{{
    *           Map(
    *             "minimum clause size" -> ?,
    *             "25th %ile clause size" -> ?,
    *             "mean clause size" -> ?,
    *             "median clause size" -> ?,
    *             "75th %ile clause size" -> ?,
    *             "maximum clause size" -> ?
    *           )
    *         }}}
    */
  def clauseSizeStats: Map[String, Double] = {
    //call descriptive stats
    val stat = new DescriptiveStatistics()
    //add to stats
    this.getClauseSizesDepths.map(_("size")).foreach(stat.addValue)

    Map(
      "minimum clause size" -> stat.getMin,
      "25th %ile clause size" -> stat.getPercentile(25),
      "mean clause size" -> stat.getMean,
      "median clause size" -> stat.getPercentile(50),
      "75th %ile clause size" -> stat.getPercentile(75),
      "maximum clause size" -> stat.getMax
    )
  }


  /**
    * Generates basic distribution of clause parse tree depths
    * @return Values representing distribution of clause parse tree depths
    *
    *         {{{
    *           Map(
    *             "minimum clause depth" -> ?,
    *             "25th %ile clause depth" -> ?,
    *             "mean clause depth" -> ?,
    *             "median clause depth" -> ?,
    *             "75th %ile clause depth" -> ?,
    *             "maximum clause depth" -> ?
    *           )
    *         }}}
    */  def clauseDepthStats: Map[String, Double] = {
    //call descriptive stats
    val stat = new DescriptiveStatistics()
    //add to stats
    this.getClauseSizesDepths.map(_("depth")).foreach(stat.addValue)

    Map(
      "minimum clause depth" -> stat.getMin,
      "25th %ile clause depth" -> stat.getPercentile(25),
      "mean clause depth" -> stat.getMean,
      "median clause depth" -> stat.getPercentile(50),
      "75th %ile clause depth" -> stat.getPercentile(75),
      "maximum clause depth" -> stat.getMax
    )
  }


  /**
    * Finds the total number of independent and dependent clauses
    * @return Counts of independent and dependent clauses in document
    */
  //return sentences grouped by structure
  def getClauseTypeCounts: Vector[Map[String, Double]] = {
    val allTrees = td.coreNLPParseTrees.flatten
    for (tree <- allTrees) yield {
      var indCounter = 0
      var depCounter = 0
      val independentMatcher = independentClause.matcher(tree)
      val dependentMatcher = dependentClause.matcher(tree)
      val clauseMatcher = clause.matcher(tree)
      while (clauseMatcher.find) {
        if (independentMatcher.find) indCounter += 1
        else if (dependentMatcher.find) depCounter += 1
      }
      //information for each sentence
      Map(
        "independent" -> indCounter.toDouble,
        "dependent" -> depCounter.toDouble
      )
    }
  }


  /**
    * Calculates the ratio of independent and dependent clauses to total number of clauses
    * @return Percentage of clauses that are independent and percent that are dependent
    */
  def totalClauseTypeRatios: Map[String, Double] = {
    //total number of clauses
    val allClauses = this.getClauseCounts.sum
    //number of independent clauses
    val indClauses = this.getClauseTypeCounts.map(_("independent")).sum
    //number of dependent clauses
    val depClauses = this.getClauseTypeCounts.map(_("dependent")).sum

    Map(
      "% of independent clauses" -> indClauses / allClauses,
      "% of dependent clauses" -> depClauses / allClauses
    )
  }


  /**
    * Determines the type of sentence based counts of independent and dependent clauses <br>
    *     `simple` = only 1 independent clause; 0 dependent clauses <br>
    *     `compound` = 2 or more independent clauses; 0 dependent clauses <br>
    *     `complex` = only 1 independent clause; 1 or more dependent clauses <br>
    *     `compound-complex` = 2 or more independent clauses; 0 dependent clauses <br>
    *     `fragment` = none of the above
    * @return `Vector` of sentence types
    */
  //identify the type of sentence
  def getSentenceStructureTypes: Vector[String] = {
    for (sentence <- this.getClauseTypeCounts) yield {
      val indCount = sentence("independent")
      val depCount = sentence("dependent")
      //      if (sentence._1._2 == 1 && sentence._2._2 == 0) "simple"
      if (indCount == 1 && depCount == 0) "simple"
      else if (indCount == 1 && depCount >= 1) "complex"
      else if (indCount >= 2 && depCount == 0) "compound"
      else if (indCount >= 2 && depCount >= 1) "compound-complex"
      else "fragment"
    }
  }


  /**
    * Calculates percentages of each type of sentence
     * @return Percentages of each sentence type
    *
    *         {{{
    *           Map(
    *             "% of simple sentences" -> ?,
    *             "% of complex sentences" -> ?,
    *             "% of compound sentences" -> ?,
    *             "% of compound-complex sentences" -> ?,
    *             "% of fragments" -> ?
    *           )
    *         }}}
    */
  def sentenceStructureTypeStats: Map[String, Double] = {
    //call frequency
    val freq = new Frequency()
    //add to frequencies
    this.getSentenceStructureTypes.foreach(freq.addValue)

    Map(
      "% of simple sentences" -> freq.getPct("simple"),
      "% of complex sentences" -> freq.getPct("complex"),
      "% of compound sentences" -> freq.getPct("compound"),
      "% of compound-complex sentences" -> freq.getPct("compound-complex"),
      "% of fragments" -> freq.getPct("fragment")
    )
  }


  /**
    * pattern generated for detecting coordinate conjunctions using `edu.stanford.nlp.trees.tregex`
    * @see [[http://nlp.stanford.edu/manning/courses/ling289/Tregex.html]]
    */
  val coordinateConjunction = TregexPattern.compile("/(for|and|nor|but|or|yet|so)/=matchedCC [> (CC > @S) | > (IN > /S(BAR)?/)]")


  /**
    * pattern generated for detecting subordinate conjunctions using `edu.stanford.nlp.trees.tregex`
    * @see [[http://nlp.stanford.edu/manning/courses/ling289/Tregex.html]]
    */
  val subordinateConjunction = TregexPattern.compile("!/(so|for|yet)/=matchedSC [> WDT | > (IN > /S(BAR)?/) | > (WRB > (/WHADVP/ !, CC . /NP/ .. /VP/ > /S(BAR)?/))]")
  //matches any subordinate conjunction that is immediately dominated by an SBAR


  /**
    * pattern generated for detecting conjunctive adverbs using `edu.stanford.nlp.trees.tregex`
    * @see [[http://nlp.stanford.edu/manning/courses/ling289/Tregex.html]]
    * @todo Current pattern simply catches things that are techincally subordinate conjunctions
    */
  val conjunctiveAdverb = TregexPattern.compile("__=matchedCA > (/RB/ > ( /.*ADVP/ !, CC . /NP/ .. /VP/ > /S(BAR)?/))")
  //matches when an adverb is used to begin the SBAR or S


  /**
    * Find matching conjunctions
    * @param rule [[coordinateConjunction]], [[subordinateConjunction]], or [[conjunctiveAdverb]]
    * @return `Vector` of matching conjunctions and node name (label given to node in `Tregex`
    */
  def getMatchingConjunctions(rule: edu.stanford.nlp.trees.tregex.TregexPattern): Vector[Vector[(String, String)]] = {

    //extracts the nodeName from the rule
    val nodeRegex = """.*=(\w+).*""".r

    //gets nodeName used for matching
    val nodeName = rule.toString match {
      case nodeRegex(name) => name
    }

    val allTrees = td.coreNLPParseTrees.flatten

    for (tree <- allTrees) yield {

      //find matching tree (or subtree)
      val matched = rule.matcher(tree)

      //initiate buffer to hold matchers
      val treeBuffer = collection.mutable.Buffer[Option[edu.stanford.nlp.trees.Tree]]()

      //iterate through matching tree
      while (matched.findNextMatchingNode) {
        val treeMatch = Option[edu.stanford.nlp.trees.Tree](matched.getNode(nodeName))
        treeBuffer += treeMatch
      }

      //iterate through matching trees and extract matching nodes
      for (tree <- treeBuffer.toVector) yield {
        if (tree.isDefined) {
          nodeName -> tree.get.toString.toLowerCase
        } else {
          nodeName -> "no match"
        }
      }
    }
  }


  /**
    * Gets counts for each conjunction used by type
    * @return `Vector` of `Counter` for each sentence containing all conjunctions <br>
    *          `Map` of `Counter`s, one for each conjunction type
    */
  //get counts for each conjunction used and total conjunctions per sentence
  def getConjunctionCounts: (Vector[Counter[String]], Map[String, Counter[String]]) = {
    //initialize counters for total of types
    val ccCounter = new Counter[String]()
    val scCounter = new Counter[String]()
    val caCounter = new Counter[String]()
    //initialize buffer for number of conjunctions per sentence
    val buffer = collection.mutable.Buffer[Counter[String]]()

    //all conjunctions found
    val foundCoordinate = this.getMatchingConjunctions(this.coordinateConjunction)
    val foundSubordinate = this.getMatchingConjunctions(this.subordinateConjunction)
    val foundConjunctive = this.getMatchingConjunctions(this.conjunctiveAdverb)

    //iterate through sentences
    for (i <- foundCoordinate.indices) {
      //initialize counter for conjunctions in sentence
      val sentenceCounter = new Counter[String]()
      //matches per sentence -- drop node name
      val coordinate = foundCoordinate(i).map(_._2)
      val subordinate = foundSubordinate(i).map(_._2)
      val conjunctive = foundConjunctive(i).map(_._2)
      //populate counters with coordinate
      for (cc <- coordinate) {
        sentenceCounter.incrementCount(cc)
        ccCounter.incrementCount(cc)
      }
      //populate counters with subordinate
      for (sc <- subordinate) {
        sentenceCounter.incrementCount(sc)
        scCounter.incrementCount(sc)
      }
      //populate counters with conjunctive
      for (ca <- conjunctive) {
        sentenceCounter.incrementCount(ca)
        caCounter.incrementCount(ca)
      }
      //add sentenceCounter to buffer
      buffer += sentenceCounter
    }

    //return tuple
      //(vector of Counters), (Map[String, Counters])
    (
      buffer.toVector,      //list of Counters of conjunctions for each sentence
      Map(
          "coordinate" -> ccCounter,
          "subordinate" -> scCounter,
          "conjunctive" -> caCounter
      )
    )
  }


  /**
    * Generates basic distribution of conjunction use
     * @return Values representing distribution of conjunction use
    *
    *         {{{
    *           Map(
    *             "mean conjunctions per sentence" -> ?,
    *             "median conjunctions per sentence" -> ?,
    *             "maximum conjunctions per sentence" -> ?,
    *             //remaining values normalized over word count
    *             "total coordinate used" -> ?,
    *             "max coordinate used" -> ?,
    *             "total subordinate used" -> ?,
    *             "max subordinate used" -> ?,
    *             "total conjunctive used" -> ?,
    *             "max conjunctive used" -> ?
    *           )
    *         }}}
    */
  def conjunctionStats: Map[String, Double] = {

    //call previous method
    val (conjunctionsPerSentenceCounters, conjunctionMap) = this.getConjunctionCounts

    //get counts per sentence
    val conjunctionsPerSentence = conjunctionsPerSentenceCounters.map(_.values.sum)

    //call descriptive stats
    val stat = new DescriptiveStatistics()
    //add to stats
    conjunctionsPerSentence.foreach(stat.addValue)

    //conjunction values
    val totalCo = conjunctionMap("coordinate")
    val maxCo = if (totalCo.values.sum == 0d) {     //if no coordinates found
                  0d
                } else {
                  totalCo.argMax._2
                }

    val totalSub = conjunctionMap("subordinate")
    val maxSub = if (totalSub.values.sum == 0d) {   //if no subordinates found
                    0d
                  } else {
                    totalSub.argMax._2
                  }

    val totalCa = conjunctionMap("conjunctive")
    val maxCa = if (totalCa.values.sum == 0d) {     //if no conjunctives found
      0d
    } else {
      totalCa.argMax._2
    }

    //normalized over total word count
    Map(
      "mean conjunctions per sentence" -> stat.getMean,
      "median conjunctions per sentence" -> stat.getPercentile(50),
      "maximum conjunctions per sentence" -> stat.getMax,
      "total coordinate used" -> totalCo.values.sum / td.totalCount("word"),  
      "max coordinate used" -> maxCo / td.totalCount("word"),
      "total subordinate used" -> totalSub.values.sum / td.totalCount("word"),
      "max subordinate used" -> maxSub / td.totalCount("word"),
      "total conjunctive used" -> totalCa.values.sum / td.totalCount("word"),
      "max conjunctive used" -> maxCa / td.totalCount("word")
    )
  }


  /**
    * pattern generated for detecting all verb phrases using `edu.stanford.nlp.trees.tregex`
    * @see [[http://nlp.stanford.edu/manning/courses/ling289/Tregex.html]]
    */  val allVPs = TregexPattern.compile("VP < /(VB$)|(VB[^G])/ !< VP")

  /**
    * pattern generated for detecting instances of passive voice using `edu.stanford.nlp.trees.tregex`
    * @see [[http://nlp.stanford.edu/manning/courses/ling289/Tregex.html]]
    */  val passive = TregexPattern.compile("!been=vbn > (VBN [>> (VP $-- (/VB/ < /('s)|(is)|(are)|(was)|(were)|(be)/=vb)) ?$.. (/PP/ <+ (IN) by <` /NP/=agent)])")


  /**
    * Finds the parse tree of all verb phrases for each sentence
    * @return `Vector` of `edu.stanford.nlp.trees.Tree`s
    */
  def getAllVPsParse: Vector[Vector[Option[edu.stanford.nlp.trees.Tree]]] = {

    val allTrees = td.coreNLPParseTrees.flatten

    //initialize buffer for capturing all VP trees
    val vpBuffer = collection.mutable.Buffer[Vector[Option[edu.stanford.nlp.trees.Tree]]]()

    //iterate through all trees
    for (tree <- allTrees) yield {

      //find matching tree (or subtree)
      val matched = this.allVPs.matcher(tree)

      //initialize buffer to hold matchers
      val matchingBuffer = collection.mutable.Buffer[Option[edu.stanford.nlp.trees.Tree]]()

      //iterate through matching tree
      while (matched.findNextMatchingNode) {
        matchingBuffer += Option[edu.stanford.nlp.trees.Tree](matched.getMatch)
      }

      //add all matches to VP buffer
      vpBuffer += matchingBuffer.toVector
    }

    //return matching VPs
    vpBuffer.toVector
  }


  /**
    * Finds the parse tree of any verb phrase that is passive
     * @return `Vector` of matching pieces of passive voice construction <br>
    *          `._1` = `VB` <br>
    *          `._2` = `VBN` <br>
    *          `._3` = `AGENT`
    */
  def getPassiveVoiceParse: Vector[Vector[(String, String, String)]] = {

    val allTrees = td.coreNLPParseTrees.flatten

    //node names
    val nodeVBN = "vbn"
    val nodeVB = "vb"
    val nodeAgent = "agent"

    //iterate through trees
    for (tree <- allTrees) yield {

      //find matching tree (or subtree)
      val matched = this.passive.matcher(tree)

      //initialize buffer to hold matchers
      val matchBuffer = collection.mutable.Buffer[
        (Option[edu.stanford.nlp.trees.Tree],       //vb
          Option[edu.stanford.nlp.trees.Tree],      //vbn
          Option[edu.stanford.nlp.trees.Tree])      //agent
      ]()

      //iterate through matches
      while (matched.findNextMatchingNode) {
        val vbn = Option[edu.stanford.nlp.trees.Tree](matched.getNode("vbn"))
        val vb = Option[edu.stanford.nlp.trees.Tree](matched.getNode("vb"))
        val agent = Option[edu.stanford.nlp.trees.Tree](matched.getNode("agent"))

        //add matches to buffer
        matchBuffer += ((vb, vbn, agent))
      }

      //normalize the outputs for vb, vbn, and agent
      for (m <- matchBuffer.toVector) yield {
        (
          if (m._1.isDefined) m._1.get.toString.toLowerCase else "",
          if (m._2.isDefined) m._2.get.toString.toLowerCase else "",
          if (m._3.isDefined) "by " + m._3.get.toString else ""
          )
      }
    }
  }


  /**
    * Generates percentage of verb phrases that are passive
    * @return Percentage of passive voice usage
    */
  //gets percentage of passive voice used
  def voiceStatsParse: Double = {
    //all VPs
    val all = this.getAllVPsParse.
      flatten.          //removes sentence boundaries and empty entries
      length.toDouble
    //passive VPs
    val passive = this.getPassiveVoiceParse.
      flatten.          //removes sentence boundaries and empty entries
      length.toDouble
    
    passive / all
  }


  //TODO add coherence
    //TODO requires W2V


  /**
    * Generates `Vector` of all features for [[TextDocument]] <br>
    *   First two items in `Vector` are `title` and `grade level`
    */
  def makeSyntacticFeatureVector: Vector[(String, Double)] = {
    Vector(
      td.title.getOrElse("") -> 0.0,
      td.gradeLevel.getOrElse("") -> 0.0,
      //sentence length
      "minimum sentence length" -> this.sentenceLengthStats("sentence length minimum"),
      "25th %ile sentence length" -> this.sentenceLengthStats("25th %ile sentence length"),
      "mean sentence length" -> this.sentenceLengthStats("sentence length mean"),
      "median sentence length" -> this.sentenceLengthStats("sentence length median"),
      "75th %ile sentence length" -> this.sentenceLengthStats("75th %ile sentence length"),
      "maximum sentence length" -> this.sentenceLengthStats("sentence length maximum"),
      //surplus punctuation
      "percent of sentences with surplus punctuation" -> this.punctuationStats("percent of sentences with surplus punctuation"),
      "25th %ile surplus punctuation size" -> this.punctuationStats("25th %ile surplus punctuation size"),
      "mean surplus punctuation size" -> this.punctuationStats("mean surplus punctuation size"),
      "median surplus punctuation size" -> this.punctuationStats("median surplus punctuation size"),
      "75th %ile surplus punctuation size" -> this.punctuationStats("75th %ile surplus punctuation size"),
      "maximum surplus punctuation size" -> this.punctuationStats("maximum surplus punctuation size"),
      //tree size
      "minimum tree size" -> this.treeSizeStats("minimum tree size"),
      "25th %ile tree size" -> this.treeSizeStats("25th %ile tree size"),
      "mean tree size" -> this.treeSizeStats("mean tree size"),
      "median tree size" -> this.treeSizeStats("median tree size"),
      "75th %ile tree size" -> this.treeSizeStats("75th %ile tree size"),
      "maximum tree size" -> this.treeSizeStats("maximum tree size"),
      //tree depthc
      "minimum tree depth" -> this.treeDepthStats("minimum tree depth"),
      "25th %ile tree depth" -> this.treeDepthStats("25th %ile tree depth"),
      "mean tree depth" -> this.treeDepthStats("mean tree depth"),
      "median tree depth" -> this.treeDepthStats("median tree depth"),
      "75th %ile tree depth" -> this.treeDepthStats("75th %ile tree depth"),
      "maximum tree depth" -> this.treeDepthStats("maximum tree depth"),
      //distance to verb      
      "minimum distance to verb" -> this.distanceToVerbStats("minimum distance to verb"),
      "25th %ile distance to verb" -> this.distanceToVerbStats("25th %ile distance to verb"),
      "mean distance to verb" -> this.distanceToVerbStats("mean distance to verb"),
      "median distance to verb" -> this.distanceToVerbStats("median distance to verb"),
      "75th %ile distance to verb" -> this.distanceToVerbStats("75th %ile distance to verb"),
      "maximum distance to verb" -> this.distanceToVerbStats("maximum distance to verb"),
      //number of constituents
      "minimum number of constituents in a sentence" -> this.constituentCountStats("minimum number of constituents in a sentence"),
      "25th %ile number of constituents in a sentence" -> this.constituentCountStats("25th %ile number of constituents in a sentence"),
      "mean number of constituents in a sentence" -> this.constituentCountStats("mean number of constituents in a sentence"),
      "median number of constituents in a sentence" -> this.constituentCountStats("median number of constituents in a sentence"),
      "75th %ile number of constituents in a sentence" -> this.constituentCountStats("75th %ile number of constituents in a sentence"),
      "maximum number of constituents in a sentence" -> this.constituentCountStats("maximum number of constituents in a sentence"),
      //constituent lengths
      "minimum constituent length" -> this.constituentSizeStats("constituent length minimum"),
      "25th %ile constituent length" -> this.constituentSizeStats("25th %ile constituent length"),
      "mean constituent length" -> this.constituentSizeStats("constituent length mean"),
      "median constituent length" -> this.constituentSizeStats("constituent length median"),
      "75th %ile constituent length" -> this.constituentSizeStats("75th %ile constituent length"),
      "maximum constituent length" -> this.constituentSizeStats("constituent length maximum"),
      //average number of clauses per sentence
      "average number of clauses per sentence" -> this.getSentenceComplexityScore,
      //clause size
      "minimum clause size" -> this.clauseSizeStats("minimum clause size"),
      "25th %ile clause size" -> this.clauseSizeStats("25th %ile clause size"),
      "mean clause size" -> this.clauseSizeStats("mean clause size"),
      "median clause size" -> this.clauseSizeStats("median clause size"),
      "75th %ile clause size" -> this.clauseSizeStats("75th %ile clause size"),
      "maximum clause size" -> this.clauseSizeStats("maximum clause size"),
      //clause depth
      "minimum clause depth" -> this.clauseDepthStats("minimum clause depth"),
      "25th %ile clause depth" -> this.clauseDepthStats("25th %ile clause depth"),
      "mean clause depth" -> this.clauseDepthStats("mean clause depth"),
      "median clause depth" -> this.clauseDepthStats("median clause depth"),
      "75th %ile clause depth" -> this.clauseDepthStats("75th %ile clause depth"),
      "maximum clause depth" -> this.clauseDepthStats("maximum clause depth"),
      //ratio of clause types
      "% of independent clauses" -> this.totalClauseTypeRatios("% of independent clauses"),
      "% of dependent clauses" -> this.totalClauseTypeRatios("% of dependent clauses"),
      //ratio of sentence types
      "% of simple sentences" -> this.sentenceStructureTypeStats("% of simple sentences"),
      "% of complex sentences" -> this.sentenceStructureTypeStats("% of complex sentences"),
      "% of compound sentences" -> this.sentenceStructureTypeStats("% of compound sentences"),
      "% of compound-complex sentences" -> this.sentenceStructureTypeStats("% of compound-complex sentences"),
      "% of fragments" -> this.sentenceStructureTypeStats("% of fragments"),
      //conjunction use
      "mean conjunctions per sentence" -> conjunctionStats("mean conjunctions per sentence"),
      "median conjunctions per sentence" -> conjunctionStats("median conjunctions per sentence"),
      "maximum conjunctions per sentence" -> conjunctionStats("maximum conjunctions per sentence"),
      "total coordinate conjunctions used" -> conjunctionStats("total coordinate used"),
      "max coordinate used" -> conjunctionStats("max coordinate used"),
      "total subordinate used" -> conjunctionStats("total subordinate used"),
      "max subordinate used" -> conjunctionStats("max subordinate used"),
      "total conjunctive used" -> conjunctionStats("total conjunctive used"),
      "max conjunctive used" -> conjunctionStats("max conjunctive used")
    )
  }


}

