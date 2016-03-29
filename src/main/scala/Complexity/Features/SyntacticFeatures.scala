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
 * Class of syntactic features contributing to ultimate feature selection.
  *
  * @param td a TextDocument
 */
class SyntacticFeatures(val td: TextDocument) {

  //sentence lengths
    //not including punctuation
  def getSentenceLengths: Vector[Double] = {
    val allSentences = this.td.words(withPunctuation = false).flatten  //remove paragraphs

    allSentences.map(_.length.toDouble)
  }


  //sentence length stats
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


  //get punctuation
  def getPunctuation: Vector[Vector[String]] = {
    val sentences = td.words(withPunctuation = true).flatten    //remove paragraphs

    //filter to only keep punctuation
    for (sent <- sentences) yield {
      sent.filterNot(token => token.matches("[A-z0-9]+"))
    }

  }


  //punctuation stats
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


  //parse tree sizes
  def getTreeSizes: Vector[Double] = {
    td.coreNLPParseTrees.
      flatten.              //remove paragraphs
      map(_.size.toDouble)
  }


  //parse tree size stats
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


  //parse tree depths
  def getTreeDepths: Vector[Double] = {
    td.coreNLPParseTrees.
      flatten.                //remove paragraphs
      map(_.depth.toDouble)
  }

  //parse tree depth stats
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



  //get distances to verb
    //uses CollinsHeadFinder to identify the root of sentence (presumably the verb) and uses its index
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


  //distance to verb stats
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


  //constituent counts
    //number of constituents per sentence
  def getConstituentCounts: Vector[Double] = {
    td.rawConstituents.
      flatten.            //remove paragraphs
      map(con =>          //for each constituent
      con.size.toDouble)  //get its size
  }


  //constituent count stats
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


  //get constituent sizes
    //number of tokens per constituent
  def getConstituentSizes: Vector[Double] = {
    //convert constituents to Scala vectors
    val consScala = td.rawConstituents.
                      flatten.                  //remove paragraphs
                      flatMap(con =>
                        con.asScala.toVector)   //cast as Scala

    //get size of constituent
    consScala.map(_.size.toDouble)

  }


  //constituent size stats
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



  //Tregex patterns for clauses
  //modified from http://personal.psu.edu/xxl13/papers/Lu_inpress_ijcl.pdf

  val clause = TregexPattern.compile("S [< (VP < (VP . CC <# MD|VBD|VBP|VBZ)) | < (VP <# MD|VBD|VBP|VBZ)]")
  //matches any S node that either (1) dominates a VP whose head is a finite verb or (2) dominates a VP consisting of conjoined VPs whose head is a finite verb

  val fragment = TregexPattern.compile("ROOT !<< VP")
  //matches any tree without a VP

  val independentClause = TregexPattern.compile("S !> SBAR [< (VP < (VP . CC <# MD|VBD|VBP|VBZ)) | < (VP <# MD|VBD|VBP|VBZ)]")
  //matches any S node that is a clause but is NOT dominated by an SBAR

  val dependentClause = TregexPattern.compile("S > SBAR [< (VP < (VP . CC <# MD|VBD|VBP|VBZ)) | < (VP <# MD|VBD|VBP|VBZ)]")
  //matches any S node that is a clause that IS dominated by an SBAR



  //total number of clauses
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


  //# of clauses / # of sentences
  def getSentenceComplexityScore: Double = {
    this.getClauseCounts.sum / td.totalSentences
  }


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


  //clause size stats
  def clauseSizeStats(filterOut: Boolean): Map[String, Double] = {
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


  //clause depth stats
  def clauseDepthStats(filterOut: Boolean): Map[String, Double] = {
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


  //ratios of clause types
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


  //sentence structure type stats
  def sentenceStructureTypeStats: Map[String, Double] = {
    //call frequency
    val freq = new Frequency()
    //add to frequencies
    this.getSentenceStructureTypes.foreach(freq.addValue)

    Map(
      "ratio of simple sentences" -> freq.getPct("simple"),
      "ratio of complex sentences" -> freq.getPct("complex"),
      "ratio of compound sentences" -> freq.getPct("compound"),
      "ratio of compound-complex sentences" -> freq.getPct("compound-complex"),
      "ratio of fragments" -> freq.getPct("fragment")
    )
  }


  //Tregex patterns for conjunction use

  val coordinateConjunction = TregexPattern.compile("/(for|and|nor|but|or|yet|so)/=matchedCC [> (CC > @S) | > (IN > /S(BAR)?/)]")

  //removes caught "for" and "yet" and "so" from CC's instead of coordinateConjunction rule
  val subordinateConjunction = TregexPattern.compile("!/(so|for|yet)/=matchedSC [> WDT | > (IN > /S(BAR)?/) | > (WRB > (/WHADVP/ !, CC . /NP/ .. /VP/ > /S(BAR)?/))]")
  //matches any subordinate conjunction that is immediately dominated by an SBAR


  //just catching things that should be subordinate conjunctions
  val conjunctiveAdverb = TregexPattern.compile("__=matchedCA > (/RB/ > ( /.*ADVP/ !, CC . /NP/ .. /VP/ > /S(BAR)?/))")
  //matches when an adverb is used to begin the SBAR or S


  //find all conjunctions using the rules above
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


  //conjunction use stats
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

    Map(
      "mean conjunctions per sentence" -> stat.getMean,
      "median conjunctions per sentence" -> stat.getPercentile(50),
      "maximum conjunctions per sentence" -> stat.getMax,
      "total coordinate used" -> totalCo.values.sum,
      "max coordinate used" -> maxCo,
      "total subordinate used" -> totalSub.values.sum,
      "max subordinate used" -> maxSub,
      "total conjunctive used" -> totalCa.values.sum,
      "max conjunctive used" -> maxCa
    )
  }


  //coherence
    //measured as the coherence between two neighboring sentences



  //use of passive voice










  /*

    def makeSyntacticFeatureVector: Vector[(String, Double)] = {
      Vector(
        (textDocument.title, 0.0),
        (textDocument.gradeLevel, 0.0),
        ("average number of conjunctions used per sentence", this.conjunctionFrequency),
        ("minimum sentence length", this.sentenceLengthStats("sentence length minimum")),
        ("25th %ile sentence length", this.sentenceLengthStats("25th %ile sentence length")),
        ("mean sentence length", this.sentenceLengthStats("sentence length mean")),
        ("median sentence length", this.sentenceLengthStats("sentence length median")),
        ("75th %ile sentence length", this.sentenceLengthStats("75th %ile sentence length")),
        ("maximum sentence length", this.sentenceLengthStats("sentence length maximum")),
        ("minimum tree size", this.treeSizeStats("minimum tree size")),
        ("25th %ile tree size", this.treeSizeStats("25th %ile tree size")),
        ("mean tree size", this.treeSizeStats("mean tree size")),
        ("median tree size", this.treeSizeStats("median tree size")),
        ("75th %ile tree size", this.treeSizeStats("75th %ile tree size")),
        ("maximum tree size", this.treeSizeStats("maximum tree size")),
        ("minimum tree depth", this.treeDepthStats("minimum tree depth")),
        ("25th %ile tree depth", this.treeDepthStats("25th %ile tree depth")),
        ("mean tree depth", this.treeDepthStats("mean tree depth")),
        ("median tree depth", this.treeDepthStats("median tree depth")),
        ("75th %ile tree depth", this.treeDepthStats("75th %ile tree depth")),
        ("maximum tree depth", this.treeDepthStats("maximum tree depth")),
        ("minimum distance to verb", this.distanceToVerbStats("minimum distance to verb")),
        ("25th %ile distance to verb", this.distanceToVerbStats("25th %ile distance to verb")),
        ("mean distance to verb", this.distanceToVerbStats("mean distance to verb")),
        ("median distance to verb", this.distanceToVerbStats("median distance to verb")),
        ("75th %ile distance to verb", this.distanceToVerbStats("75th %ile distance to verb")),
        ("maximum distance to verb", this.distanceToVerbStats("maximum distance to verb")),
        ("minimum number of constituents in a sentence", this.constituentCountStats("minimum number of constituents in a sentence")),
        ("25th %ile number of constituents in a sentence", this.constituentCountStats("25th %ile number of constituents in a sentence")),
        ("mean number of constituents in a sentence", this.constituentCountStats("mean number of constituents in a sentence")),
        ("median number of constituents in a sentence", this.constituentCountStats("median number of constituents in a sentence")),
        ("75th %ile number of constituents in a sentence", this.constituentCountStats("75th %ile number of constituents in a sentence")),
        ("maximum number of constituents in a sentence", this.constituentCountStats("maximum number of constituents in a sentence")),
        ("minimum constituent length", this.constituentLengthStats("constituent length minimum")),
        ("25th %ile constituent length", this.constituentLengthStats("25th %ile constituent length")),
        ("mean constituent length", this.constituentLengthStats("constituent length mean")),
        ("median constituent length", this.constituentLengthStats("constituent length median")),
        ("75th %ile constituent length", this.constituentLengthStats("75th %ile constituent length")),
        ("maximum constituent length", this.constituentLengthStats("constituent length maximum")),
        ("average number of clauses per sentence", this.getSentenceComplexityScore),
        ("% of simple sentences", this.sentenceStructureTypeStats("ratio of simple sentences")),
        ("% of complex sentences", this.sentenceStructureTypeStats("ratio of complex sentences")),
        ("% of compound sentences", this.sentenceStructureTypeStats("ratio of compound sentences")),
        ("% of compound-complex sentences", this.sentenceStructureTypeStats("ratio of compound-complex sentences")),
        ("% of fragments", this.sentenceStructureTypeStats("ratio of fragments"))
      )
    }

    */
}

