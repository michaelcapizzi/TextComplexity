package Complexity.Features

import java.util
import Complexity.TextDocument
import edu.stanford.nlp.trees.tregex.TregexPattern
import edu.stanford.nlp.trees.CollinsHeadFinder
import org.apache.commons.math3.stat.Frequency
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
import scala.collection.JavaConverters._



/**
 * Class of syntactic features contributing to ultimate feature selection.
  * Takes a TextDocument as input.
 */


class SyntacticFeatures(val td: TextDocument) {

  /*def getSentences: Vector[Vector[String]] = {
    this.textDocument.textDoc.flatMap(_.sentences.map
      (_.words.toVector))
  }*/

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


  //TODO conjunction usage


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


  /*




  //Tregex patterns
  //modified from http://personal.psu.edu/xxl13/papers/Lu_inpress_ijcl.pdf

  val clause = TregexPattern.compile("S [< (VP < (VP . CC <# MD|VBD|VBP|VBZ)) | < (VP <# MD|VBD|VBP|VBZ)]")
  //matches any S node that either (1) dominates a VP whose head is a finite verb or (2) dominates a VP consisting of conjoined VPs whose head is a finite verb

  val fragment = TregexPattern.compile("ROOT !<< VP")
  //matches any tree without a VP

  val independentClause = TregexPattern.compile("S !> SBAR [< (VP < (VP . CC <# MD|VBD|VBP|VBZ)) | < (VP <# MD|VBD|VBP|VBZ)]")
  //matches any S node that is a clause but is NOT dominated by an SBAR

  val dependentClause = TregexPattern.compile("S > SBAR [< (VP < (VP . CC <# MD|VBD|VBP|VBZ)) | < (VP <# MD|VBD|VBP|VBZ)]")
  //matches any S node that is a clause that IS dominated by an SBAR


  def getClauseCounts: Vector[Double] = {
    for (tree <- this.getParseTrees) yield {
      var counter = 0
      val clauseMatcher = clause.matcher(tree)
      while (clauseMatcher.find) {
        counter += 1
      }
      counter.toDouble
    }
  }

  //TODO build method -- see PitchVantage code?
  def getClauseLengths = {
    //how to do?
    //use loop through and extract + tree.size
  }

  //# of clauses / # of sentences
  def getSentenceComplexityScore: Double = {
    this.getClauseCounts.sum / textDocument.sentenceSize
  }

  //return sentences grouped by structure
  def getClauseTypeCounts: Vector[((String, Int), (String, Int))] = {
    val trees = this.getParseTrees
    for (tree <- trees) yield {
      var indCounter = 0
      var depCounter = 0
      val independentMatcher = independentClause.matcher(tree)
      val dependentMatcher = dependentClause.matcher(tree)
      val clauseMatcher = clause.matcher(tree)
      while (clauseMatcher.find) {
        if (independentMatcher.find) indCounter += 1
        else if (dependentMatcher.find) depCounter += 1
      }
      ("independent clauses" -> indCounter, "dependent clauses" -> depCounter)
    }
  }

  def getSentenceStructureTypes: Vector[String] = {
    this.getClauseTypeCounts.map(sentence =>
      if (sentence._1._2 == 1 && sentence._2._2 == 0) "simple"
      else if (sentence._1._2 == 1 && sentence._2._2 >= 1) "complex"
      else if (sentence._1._2 >= 2 && sentence._2._2 == 0) "compound"
      else if (sentence._1._2 >= 2 && sentence._2._2 >= 1) "compound-complex"
      else "fragment"
    )
  }

  def sentenceStructureTypeStats: Map[String, Double] = {
    val freq = new Frequency()
    this.getSentenceStructureTypes.foreach(freq.addValue)            //count

    Map(
      "ratio of simple sentences" -> freq.getPct("simple"),
      "ratio of complex sentences" -> freq.getPct("complex"),
      "ratio of compound sentences" -> freq.getPct("compound"),
      "ratio of compound-complex sentences" -> freq.getPct("compound-complex"),
      "ratio of fragments" -> freq.getPct("fragment")
    )
  }


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

