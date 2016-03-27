package Complexity

import edu.arizona.sista.struct.Counter
import java.util
import TextDocument._

import scala.collection.parallel.ParSeq


/**
  * Created by mcapizzi on 3/25/16.
  */
class TextDocument (
                    val paragraphs: Vector[ProcessedParagraph]    //already annotated!
                   ){

  //take title from any paragraph
  val title = paragraphs.map(_.title).find(z => z.nonEmpty).getOrElse("")

  //take author from any paragraph
  val author = paragraphs.map(_.author).find(z => z.nonEmpty).getOrElse("")

  //take grade level from any paragraph
  val gradeLevel = paragraphs.map(_.author).find(z => z.nonEmpty).getOrElse("")

  //parallelized vector of paragraphs
  val parParagraphs = this.paragraphs.par

 /* //annotate all paragraphs
    //parallelizing causes error in discourse parse - why?
  def annotateAll: Unit = {
    this.paragraphs.foreach(_.annotate)
  }*/

  val allCounters = this.parParagraphs.map(_.buildCounters)
  val stringCounters = allCounters.map(_._1)
  val stringStringCounters = allCounters.map(_._2)

  val tokensCounter = foldApply[Counter[String]](
                        stringCounters.map(_("tokens")),
                        new Counter[String](),
                        (a: Counter[String], b: Counter[String]) => a + b
                      )

  val lemmasCounter = foldApply[Counter[String]](
                        stringCounters.map(_("lemmas")),
                        new Counter[String](),
                        (a: Counter[String], b: Counter[String]) => a + b
                      )

  val tagsCounter = foldApply[Counter[String]](
                        stringCounters.map(_("tags")),
                        new Counter[String](),
                        (a: Counter[String], b: Counter[String]) => a + b
                      )

  val tokensTagsCounter = foldApply[Counter[(String, String)]](
                        stringStringCounters.map(_("tokens-tags")),
                        new Counter[(String, String)](),
                        (a: Counter[(String, String)], b: Counter[(String, String)]) => a + b
                      )

  val lemmasTagsCounter = foldApply[Counter[(String, String)]](
                        stringStringCounters.map(_("lemmas-tags")),
                        new Counter[(String, String)](),
                        (a: Counter[(String, String)], b: Counter[(String, String)]) => a + b
                      )

  val properNounsCounter = foldApply[Counter[String]](
                        stringCounters.map(_("proper nouns")),
                        new Counter[String](),
                        (a: Counter[String], b: Counter[String]) => a + b
                      )


  //get counter of specific POS
  //gram = "word" or "lemma"
  //TODO took wrote regex and then only had to do counter steps once
  //TODO initialize variable outside of loop
  def filterCounterByPOS(POS: String, gram: String): Counter[(String, String)] = {
    //initiate regex
    var regex = ""

    POS match {
      case "noun" => regex = "NN.?"
      case "adjective" => regex = "JJ.?"
      case "verb" => regex = "VB.?"
      case "adverb" => regex = "RB.?"
      case _ => regex = "NN.?"
    }

    //filter counter
    gram match {
      case "word" => this.tokensTagsCounter.filter(t => t._1._2.matches(regex))
      case "lemma" => this.lemmasTagsCounter.filter(t => t._1._2.matches(regex))
      case _ => this.tokensTagsCounter.filter(t => t._1._2.matches(regex))
    }
  }


  //keeps only nouns, adjectives, verbs, and adverbs
  def filterStopWords(gram: String): Counter[(String, String)] = {

    val nounCounter = this.filterCounterByPOS("noun", gram)
    val verbCounter = this.filterCounterByPOS("verb", gram)
    val adjCounter = this.filterCounterByPOS("adjective", gram)
    val advCounter = this.filterCounterByPOS("adverb", gram)
    val counterList = ParSeq(nounCounter, verbCounter, adjCounter, advCounter)

    foldApply
      [Counter[(String, String)]](
        counterList,
        new Counter[(String, String)](),
        (a: Counter[(String, String)], b: Counter[(String, String)]) => a + b
      )

  }


  ////////////////NLP items////////////////////

  //raw sentences
  val rawSentences = this.paragraphs.map(_.rawSentences)

  //words
  def words(withPunctuation: Boolean): Vector[Vector[Vector[String]]] = {
    this.paragraphs.map(_.words(withPunctuation))
  }

  //lemmas
  def lemmas(withPunctuation: Boolean): Vector[Vector[Vector[String]]] = {
    this.paragraphs.map(_.lemmas(withPunctuation))
  }

  //tags
  def tags(withPunctuation: Boolean): Vector[Vector[Vector[String]]] = {
    this.paragraphs.map(_.tags(withPunctuation))
  }

  //entities
  def entities(withPunctuation: Boolean): Vector[Vector[Vector[String]]] = {
    this.paragraphs.map(_.entities(withPunctuation))
  }

  //lexical tuple
  def lexicalTuples(withPunctuation: Boolean): Vector[Vector[Vector[(String, (String, String, String))]]] = {
    this.paragraphs.map(_.lexicalTuple(withPunctuation))
  }

  //parse trees SISTA
  def sistaParseTrees: Vector[Vector[edu.arizona.sista.struct.Tree]] = {
    this.paragraphs.map(_.sistaParseTree)
  }

  //parse trees CoreNLP
  def coreNLPParseTrees: Vector[Vector[edu.stanford.nlp.trees.Tree]] = {
    this.paragraphs.map(_.coreNLPParseTree)
  }

  //constituents
  def rawConstituents: Vector[Vector[util.Set[edu.stanford.nlp.trees.Constituent]]] = {
    this.paragraphs.map(_.rawConstituents)
  }

  //dependencies
  def dependencies: Vector[Vector[edu.arizona.sista.struct.DirectedGraph[String]]] = {
    this.paragraphs.map(_.rawDependencies)
  }

  //discourse parse
  def rawDiscourseParses: Vector[edu.arizona.sista.discourse.rstparser.DiscourseTree] = {
    this.paragraphs.map(_.rawDiscourseParse)
  }

  /////////////normalization terms/////////////

  //total tokens
    //gram = "word" or "lemma"
  def totalCount(gram: String): Double = {
    gram match {
      case "word" => this.tokensCounter.values.sum
      case "lemma" => this.lemmasCounter.values.sum
      case _ => this.tokensCounter.values.sum
    }
  }

  //total tokens without proper nouns
  def totalCountMinusProper: Double = {
    (this.tokensCounter - this.properNounsCounter).values.sum
  }

  //distinct tokens
    //gram = "word" or "lemma"
  def distinctCount(gram: String): Double = {
    case "word" => this.tokensCounter.size.toDouble
    case "lemma" => this.lemmasCounter.size.toDouble
    case _ => this.tokensCounter.size.toDouble
  }

  //ratio of distinct to total tokens
  def countRatio(gram: String): Double = {
    this.distinctCount(gram) / this.totalCount(gram)
  }


}

object TextDocument {

  //wrappers for fold functions (to accumulate results across all paragraphs)
  //function ([T], [T]) => [T] + [T])

  //fold function generalized
  //TODO use of Typing
  //example: foldApply(tagCounterList, new Counter[String], (a: Counter[String], b: Counter[String]) => a + b)
  def foldApply[T](list: scala.collection.parallel.ParSeq[T], startingItem: T, function: (T, T) => T): T = {
    list.foldLeft(startingItem)(function)
  }

  //TODO are these needed anymore?
  //fold function for integers
  def foldApplyInt(list: scala.collection.parallel.ParSeq[Int], startingInt: Int, function: (Int, Int) => Int): Int = {
    list.foldLeft(startingInt)(function)
  }

  //fold function for doubles
  def foldApplyDouble(list: scala.collection.parallel.ParSeq[Double], startingDouble: Double, function: (Double, Double) => Double): Double = {
    list.foldLeft(startingDouble)(function)
  }

  //fold function for counters
  def foldApplyCounter(list: scala.collection.parallel.ParSeq[Counter[String]], startingCounter: Counter[String], function: (Counter[String], Counter[String]) => Counter[String]): Counter[String] = {
    list.foldLeft(startingCounter)(function)
  }

}
