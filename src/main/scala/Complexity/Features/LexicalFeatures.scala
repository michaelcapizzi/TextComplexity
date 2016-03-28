package Complexity.Features


import Complexity.SupportMethods.Concreteness._
import Complexity.TextDocument
import Complexity.TextDocument._
import edu.arizona.sista.struct.Counter
import org.apache.commons.math3.stat.Frequency
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

import scala.collection.parallel.immutable.ParSeq


/**
 * Created by mcapizzi on 8/17/15.
 */
class LexicalFeatures (td: TextDocument) {

  //distinct percentages
  //gram = "word" or "lemma"
  def getDistinctRatios(gram: String): Map[String, Double] = {
    Map(
      "nouns" -> td.filterCounterByPOS("noun", gram).size.toDouble,
      "adjectives" -> td.filterCounterByPOS("adjective", gram).size.toDouble,
      "verbs" -> td.filterCounterByPOS("verbs", gram).size.toDouble
    )
  }


  //word lengths
  def wordLengths(keepProper: Boolean): Vector[Double] = {
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


  //word length stats
  def wordLengthStats(keepProper: Boolean): Map[String, Double] = {
    //call descriptive stats
    val stat = new DescriptiveStatistics()
    //add to stats
    this.wordLengths(keepProper).foreach(stat.addValue)
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


  //most frequent word by POS
    //gram = "word" or "lemma"
    //returns (word, count)
  //TODO made .filterCounterByPOS to be used in the following two methods
  //TODO compare to original
  def getMostFrequent(POS: String, gram: String): (String, Double) = {

    val filteredCounter = td.filterCounterByPOS(POS, gram)
    val mostFrequent = filteredCounter.topKeys(1).head  //take top value of counter

    //return (word, count)
    mostFrequent._1 -> filteredCounter.getCount(mostFrequent)
  }


  //word concreteness for all nouns, adjectives and verbs
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

    Map(
      "number of tokens present in database normalized over non-proper noun word count" -> concretenessList.length.toDouble / td.totalCountMinusProper,
      "number of tokens not present in database normalized over non-proper noun word count" -> removedNumber / td.totalCountMinusProper,
      "minimum concreteness score present in text" -> stat.getMin,
      "25th %ile concreteness score present in text" -> stat.getPercentile(25),
      "mean concreteness score present in text" -> stat.getMean,
      "median concreteness score present in text" -> stat.getPercentile(50),
      "75th %ile concreteness score present in text" -> stat.getPercentile(75),
      //"maximum concreteness score present in text" -> stat.getMax,             //only 280 items = 5; too subjective of a list to use as measure?
      "concreteness score of most used noun" -> concretenessMap.getOrElse(this.getMostFrequent("noun", "lemma")._1, 0d),
      "concreteness score of most used adjective" -> concretenessMap.getOrElse(this.getMostFrequent("adjective", "lemma")._1, 0d),
      "concreteness score of most used verb" -> concretenessMap.getOrElse(this.getMostFrequent("verb", "lemma")._1, 0d)
    )
  }


  def makeLexicalFeatureVector: Vector[(String, Double)] = {
    Vector(
      td.title.getOrElse("") -> 0d,
      td.gradeLevel.getOrElse("") -> 0d,
      "distinct word ratio" -> td.countRatio("word"),
      "distinct lemma ratio" -> td.countRatio("lemma"),
      "% of distinct nouns in all words" -> this.getDistinctRatios("word")("nouns") / td.totalCount("word"),
      "% of distinct verbs in all words" -> this.getDistinctRatios("word")("verbs") / td.totalCount("word"),
      "% of distinct adjectives in all words" -> this.getDistinctRatios("word")("adjectives") / td.totalCount("word"),
      "% of distinct nouns in all lemmas" -> this.getDistinctRatios("lemma")("nouns") / td.totalCount("lemma"),
      "% of distinct verbs in all lemmas" -> this.getDistinctRatios("lemma")("verbs") / td.totalCount("lemma"),
      "% of distinct adjectives in all lemmas" -> this.getDistinctRatios("lemma")("adjectives") / td.totalCount("lemma"),
      "minimum word length" -> this.wordLengthStats(false)("minimum word length"),
      "25th %ile word length" -> this.wordLengthStats(false)("25th %ile word length"),
      "mean word length" -> this.wordLengthStats(false)("mean word length"),
      "median word length" -> this.wordLengthStats(false)("median word length"),
      "75th %ile word length" -> this.wordLengthStats(false)("75th %ile word length"),
      "maximum word length" -> this.wordLengthStats(false)("maximum word length"),
      "% of tokens not present in concreteness" -> this.wordConcretenessStats("number of tokens not present in database normalized over non-proper noun word count"),
      "minimum concreteness score present in text" -> this.wordConcretenessStats("minimum concreteness score present in text"),
      "25th %ile concreteness score present in text" -> this.wordConcretenessStats("25th %ile concreteness score present in text"),
      "mean concreteness score present in text" -> this.wordConcretenessStats("mean concreteness score present in text"),
      "median concreteness score present in text" -> this.wordConcretenessStats("median concreteness score present in text"),
      "75th %ile concreteness score present in text" -> this.wordConcretenessStats("75th %ile concreteness score present in text"),
      "concreteness score of most used noun" -> this.wordConcretenessStats("concreteness score of most used noun"),
      "concreteness score of most used verb" -> this.wordConcretenessStats("concreteness score of most used verb"),
      "concreteness score of most used adjective" -> this.wordConcretenessStats("concreteness score of most used adjective")
    )
  }

}
