package Complexity.Features


import Complexity.SupportMethods.Concreteness._
import Complexity.TextDocument
import Complexity.TextDocument._
import edu.arizona.sista.struct.Counter
import org.apache.commons.math3.stat.Frequency
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

import scala.collection.parallel.immutable.ParSeq


/**
 * Generates features lexical in nature (at the word level)
  * @param td [[TextDocument]] for the document to be analyzed
 */
class LexicalFeatures (val td: TextDocument) {

  /**
    * Calculates percentage of distinct tokens (types) for `noun`s, `adjective`s, and `verb`s
    * @param gram `word` or `lemma`
    * @return Percentage of tokens that are distinct
    *
    *     {{{
    *       Map(
    *         "nouns" -> ?,
    *         "adjectives" -> ?,
    *         "verbs" -> ?
    *       )
    *     }}}
    */
  def getDistinctRatios(gram: String): Map[String, Double] = {
    //initialize counter
    var allTokens = new Counter[String]()

    //populate counter based on "gram" parameter
    gram match {
      case "word" => allTokens = td.tokensCounter
      case "lemma" => allTokens = td.lemmasCounter
      case _ => allTokens = td.tokensCounter
    }

    //normalize by total words or lemmas
    Map(
      "nouns" -> td.filterCounterByPOS("noun", gram).size.toDouble / allTokens.size.toDouble,
      "adjectives" -> td.filterCounterByPOS("adjective", gram).size.toDouble / allTokens.size.toDouble,
      "verbs" -> td.filterCounterByPOS("verbs", gram).size.toDouble / allTokens.size.toDouble
    )
  }


  /***
    * Gets length of each word in document
     * @param keepProper Use of `true` will keep proper nouns in the calculation <br> `false` will remove proper nouns from calculation
    * @return `Vector` of word lengths
    */
  def getWordLengths(keepProper: Boolean): Vector[Double] = {
    if (keepProper) {
      for (word <- td.tokensCounter.keySet.toVector) yield {
        word.length.toDouble
      }
    } else {
      //remove proper nouns from counter
      val minusProperCounter = td.tokensCounter - td.properNounsCounter
      for (word <- minusProperCounter.keySet.toVector) yield {
        word.length.toDouble
      }
    }
  }


  /**
    * Generates basic distribution of word lengths
     * @param keepProper Use of `true` will keep proper nouns in the calculation <br> `false` will remove proper nouns from calculation
    * @return Values representing distribution of word lengths
    *
    *         {{{
    *           Map(
    *             "minimum word length" -> ?,
    *             "25th %ile word length" -> ?,
    *             "mean word length" -> ?,
    *             "median word length" -> ?,
    *             "75th %ile word length" -> ?,
    *             "maximum word length" -> ?
    *           )
    *         }}}
    */
  def wordLengthStats(keepProper: Boolean): Map[String, Double] = {
    //call descriptive stats
    val stat = new DescriptiveStatistics()
    //add to stats
    this.getWordLengths(keepProper).foreach(stat.addValue)
    //stats
    Map(
      "minimum word length" -> stat.getMin,
      "25th %ile word length" -> stat.getPercentile(25),
      "mean word length" -> stat.getMean,
      "median word length" -> stat.getPercentile(50),
      "75th %ile word length" -> stat.getPercentile(75),
      "maximum word length" -> stat.getMax
    )
  }


  /**
    * Finds the most frequent token for a given part of speech
    * @param POS `noun`, `adjective`, `verb`, or `adverb`
    * @param gram `word` or `lemma`
    * @return `(token, count of that token)`
    */
  //TODO made .filterCounterByPOS to be used in the following two methods
  //TODO compare to original
  def getMostFrequent(POS: String, gram: String): (String, Double) = {

    val filteredCounter = td.filterCounterByPOS(POS, gram)
    val mostFrequent = filteredCounter.topKeys(1).head  //take top value of counter

    //return (word, count)
    mostFrequent._1 -> filteredCounter.getCount(mostFrequent)
  }


  /**
    * Finds the concreteness score (as generated by humans in a Psychology experiment) for all `noun`s, `verb`s, `adjective`s, and `adverb`s in the document <br>
    *   Scores are from 1 (very abstract) to 5 (very concrete)
    * @see `.csv` file of scores can be found here: [[https://github.com/michaelcapizzi/TextComplexity/blob/master/src/main/resources/concretenessData.csv]]
    * @return `Vector` of concreteness scores
    */
  def getWordConcreteness: Vector[(String, Double)] = {
    val allTokens = td.filterStopWords("lemma").
                      keySet.
                      map(_._1).                  //keep just the lemma
                      toVector

    for (l <- allTokens) yield {
      (
        l,                                      //the lemma
        concretenessMap.getOrElse(l, 99d)       //its concreteness score (0 - 5; 5 very concrete; 99 not in database)
      )
    }
  }


  /**
    * Generates basic distribution of concreteness scores
    * @return Values representing distribution of concreteness scores
    *
    *         {{{
    *           Map(
    *             "number of tokens present in database" -> ?,
    *             "number of tokens not present in database" -> ?,
    *             "minimum concreteness score present in text" -> ?,
    *             "25th %ile concreteness score present in text" -> ?,
    *             "mean concreteness score present in text" -> ?,
    *             "median concreteness score present in text" -> ?,
    *             "75th %ile concreteness score present in text" -> ?,
    *             "concreteness score of most used noun" -> ?,
    *             "concreteness score of most used adjective" -> ?,
    *             "concreteness score of most used verb" -> ?,
    *             "concreteness score of most used adverb" -> ?,
    *           )
    *         }}}
    */
  def wordConcretenessStats: Map[String, Double] = {
    //concreteness list
    val concretenessList = this.getWordConcreteness
    //concreteness map
    val concretenessMap = concretenessList.toMap

    val stat = new DescriptiveStatistics()
    //items not in database
    val removed = concretenessList.filter(item => item._2 == 99d)
    //number of items removed
    val removedNumber = concretenessList.length.toDouble - removed.length.toDouble
    //count items
    removed.foreach(tuple => stat.addValue(tuple._2))

    //normalized when necessary of non-proper noun word count
    Map(
      "number of tokens present in database" -> concretenessList.length.toDouble / td.totalCountMinusProper,
      "number of tokens not present in database" -> removedNumber / td.totalCountMinusProper,
      "minimum concreteness score present in text" -> stat.getMin,
      "25th %ile concreteness score present in text" -> stat.getPercentile(25),
      "mean concreteness score present in text" -> stat.getMean,
      "median concreteness score present in text" -> stat.getPercentile(50),
      "75th %ile concreteness score present in text" -> stat.getPercentile(75),
      //"maximum concreteness score present in text" -> stat.getMax,             //only 280 items = 5; too subjective of a list to use as measure?
      "concreteness score of most used noun" -> concretenessMap.getOrElse(this.getMostFrequent("noun", "lemma")._1, 0d),
      "concreteness score of most used adjective" -> concretenessMap.getOrElse(this.getMostFrequent("adjective", "lemma")._1, 0d),
      "concreteness score of most used verb" -> concretenessMap.getOrElse(this.getMostFrequent("verb", "lemma")._1, 0d),
      "concreteness score of most used adverb" -> concretenessMap.getOrElse(this.getMostFrequent("adverb", "lemma")._1, 0d)
    )
  }


  /**
    * Generates `Vector` of all features for [[TextDocument]] <br>
    *   First two items in `Vector` are `title` and `grade level`
    */
  def makeLexicalFeatureVector: Vector[(String, Double)] = {
    Vector(
      td.title.getOrElse("") -> 0d,
      td.gradeLevel.getOrElse("") -> 0d,
      //word/lemma ratios
      "distinct word ratio" -> td.countRatio("word"),
      "distinct lemma ratio" -> td.countRatio("lemma"),
      //ratio of distinct parts of speech
      "% of distinct nouns in all words" -> this.getDistinctRatios("word")("nouns") / td.totalCount("word"),
      "% of distinct verbs in all words" -> this.getDistinctRatios("word")("verbs") / td.totalCount("word"),
      "% of distinct adjectives in all words" -> this.getDistinctRatios("word")("adjectives") / td.totalCount("word"),
      "% of distinct nouns in all lemmas" -> this.getDistinctRatios("lemma")("nouns") / td.totalCount("lemma"),
      "% of distinct verbs in all lemmas" -> this.getDistinctRatios("lemma")("verbs") / td.totalCount("lemma"),
      "% of distinct adjectives in all lemmas" -> this.getDistinctRatios("lemma")("adjectives") / td.totalCount("lemma"),
      //word length
      "minimum word length" -> this.wordLengthStats(false)("minimum word length"),
      "25th %ile word length" -> this.wordLengthStats(false)("25th %ile word length"),
      "mean word length" -> this.wordLengthStats(false)("mean word length"),
      "median word length" -> this.wordLengthStats(false)("median word length"),
      "75th %ile word length" -> this.wordLengthStats(false)("75th %ile word length"),
      "maximum word length" -> this.wordLengthStats(false)("maximum word length"),
      //concreteness
      "% of tokens not present in concreteness" -> this.wordConcretenessStats("number of tokens not present in database") / (td.tokensCounter - td.properNounsCounter).values.sum,
      "minimum concreteness score present in text" -> this.wordConcretenessStats("minimum concreteness score present in text"),
      "25th %ile concreteness score present in text" -> this.wordConcretenessStats("25th %ile concreteness score present in text"),
      "mean concreteness score present in text" -> this.wordConcretenessStats("mean concreteness score present in text"),
      "median concreteness score present in text" -> this.wordConcretenessStats("median concreteness score present in text"),
      "75th %ile concreteness score present in text" -> this.wordConcretenessStats("75th %ile concreteness score present in text"),
      "concreteness score of most used noun" -> this.wordConcretenessStats("concreteness score of most used noun"),
      "concreteness score of most used verb" -> this.wordConcretenessStats("concreteness score of most used verb"),
      "concreteness score of most used adjective" -> this.wordConcretenessStats("concreteness score of most used adjective"),
      "concreteness score of most used adverb" -> this.wordConcretenessStats("concreteness score of most used adverb")
    )
  }

}
