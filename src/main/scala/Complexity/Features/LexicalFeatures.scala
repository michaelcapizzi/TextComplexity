package Complexity.Features


import Complexity.SupportMethods.Concreteness._
import Complexity.TextDocument
import org.apache.commons.math3.stat.Frequency
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics


/**
 * Created by mcapizzi on 8/17/15.
 */
class LexicalFeatures {

  //number of distinct tokens
  def getDistinctCount(tokenCounter: edu.arizona.sista.struct.Counter[String]): Double = {
    tokenCounter.size.toDouble
  }

  //number of all tokens
  def getTotalCount(tokenCounter: edu.arizona.sista.struct.Counter[String]): Double = {
//    tokenCounter.getTotal       //why throwing error? 'getTotal is not a member of Counter[String]?
    tokenCounter.values.sum
  }

  //ratio of distinct words to total words
  def getCountRatio(distinct: Double, total: Double): Double = {
    distinct / total
  }

  //word lengths
  def wordLengths(tokenCounter: edu.arizona.sista.struct.Counter[String], properNouns: Vector[String], keepProper: Boolean): Vector[Double] = {
    if (keepProper) {
      for (word <- tokenCounter.keySet.toVector) yield {
        word.length.toDouble
      }
    } else {
      //make counter of proper nouns
      val properCounter = new edu.arizona.sista.struct.Counter[String]()
      for (word <- properNouns) {
        properCounter.incrementCount(word)
      }
      //remove proper nouns from counter
      val minusProperCounter = tokenCounter - properCounter
      for (word <- minusProperCounter.keySet.toVector) yield {
        word.length.toDouble
      }
    }
  }

  //word length stats
  def wordLengthStats(allWordLengths: Vector[Double]): Map[String, Double] = {
    //call descriptive stats
    val stat = new DescriptiveStatistics()
    //add to stats
    allWordLengths.foreach(stat.addValue)
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


  //get counter of specific POS
  //TODO took wrote regex and then only had to do counter steps once
  //TODO initialize variable outside of loop
  def filterCounterByPOS(POS: String, counter: edu.arizona.sista.struct.Counter[(String, String)]): edu.arizona.sista.struct.Counter[(String, String)] = {
    //initiate regex
    var regex = ""
    //get regex for POS
    if (POS == "noun") {
      regex = "NN.?"
    } else if (POS == "adjective") {
      regex = "JJ.?"
    } else if (POS == "verb") {
      regex = "VB.?"
    } else {          //default to noun
      regex = "NN.?"
    }
    //filter counter
    counter.filter(token =>
      token._1._2.matches(regex)
    )
  }

  //most frequent word by POS
    //returns (word, count)
  //TODO made .filterCounterByPOS to be used in the following two methods
  //TODO compare to original
  def getMostFrequent(filteredCounter: edu.arizona.sista.struct.Counter[(String, String)]): (String, Double) = {

    val mostFrequent = filteredCounter.topKeys(1).head   //take top value of counter

    //return (word, count)
    mostFrequent._1 -> filteredCounter.getCount(mostFrequent)
  }



  //# of total distinct lemmas by part of speech
  //verb (VB.*)
  //adjective (JJ.*)
  //conjunctions (CC)
  def getDistinctLemmaCount(filteredCounter: edu.arizona.sista.struct.Counter[(String, String)]): Double = {
//    filteredCounter.getTotal              //why throwing error? 'getTotal is not a member of Counter[String]?
    filteredCounter.values.sum
  }

  /*

  //word concreteness
  def getWordConcreteness: Vector[(String, String)] = {
    textDocument.getLemmas.map(lemma =>                     //uses lemmas
      (
        lemma,                                      //the lemma
        concretenessMap.getOrElse(lemma, "99")      //its concreteness score (0 - 5; 5 very concrete; 99 not in database)
        )
    )
  }

  def wordConcretenessStats: Map[String, Double] = {
    val stat = new DescriptiveStatistics()
    val removed = this.getWordConcreteness.filter(missing => missing._2 == "99").distinct.length.toDouble     //count of how many distinct words weren't in database
    val concretenessDouble = this.getWordConcreteness.map(item =>                                             //process results of .getWordConcreteness
        (
          item._1,
          item._2.toDouble                                                                                    //converts concreteness score to Double
          )
      ).filterNot(missing =>
        missing._2 == 99)                                                                                     //remove words not in database
    concretenessDouble.foreach(tuple => stat.addValue(tuple._2))                                                  //count

    Map(
      "number of tokens present in database normalized over non-proper noun word count" -> concretenessDouble.length.toDouble / textDocument.wordCountMinusProperNouns,
      "number of tokens not present in database normalized over non-proper noun word count" -> removed / textDocument.wordCountMinusProperNouns,
      "minimum concreteness score present in text" -> stat.getMin,
      "25th %ile concreteness score present in text" -> stat.getPercentile(25),
      "mean concreteness score present in text" -> stat.getMean,
      "median concreteness score present in text" -> stat.getPercentile(50),
      "75th %ile concreteness score present in text" -> stat.getPercentile(75),
      //"maximum concreteness score present in text" -> stat.getMax,             //only 280 items = 5; too subjective of a list to use as measure?
      "concreteness score of most used noun" -> concretenessDouble.toMap.getOrElse(this.mostFrequentWords._1._2, 0.toDouble),
      "concreteness score of most used adjective" -> concretenessDouble.toMap.getOrElse(this.mostFrequentWords._2._2, 0.toDouble),
      "concreteness score of most used verb" -> concretenessDouble.toMap.getOrElse(this.mostFrequentWords._3._2, 0.toDouble)
    )
  }

  def makeLexicalFeatureVector: Vector[(String, Double)] = {
    Vector(
      (textDocument.title, 0.0),
      (textDocument.gradeLevel, 0.0),
      ("distinct token ratio", this.distinctTokenRatio),
      ("number of distinct conjunctions", this.countDistinctPOS("CC.*")),
      ("% of distinct nouns in all words", this.countDistinctPOS("NN.*")),
      ("% of distinct verbs in all words", this.countDistinctPOS("VB.*")),
      ("% of distinct adjectives in all words", this.countDistinctPOS("JJ.*")),
      ("minimum word length", this.wordLengthStats(false)("minimum word length")),
      ("25th %ile word length", this.wordLengthStats(false)("25th %ile word length")),
      ("mean word length", this.wordLengthStats(false)("mean word length")),
      ("median word length", this.wordLengthStats(false)("median word length")),
      ("75th %ile word length", this.wordLengthStats(false)("75th %ile word length")),
      ("maximum word length", this.wordLengthStats(false)("maximum word length")),
      ("% of tokens not present in concreteness", this.wordConcretenessStats("number of tokens not present in database normalized over non-proper noun word count")),
      ("minimum concreteness score present in text", this.wordConcretenessStats("minimum concreteness score present in text")),
      ("25th %ile concreteness score present in text", this.wordConcretenessStats("25th %ile concreteness score present in text")),
      ("mean concreteness score present in text", this.wordConcretenessStats("mean concreteness score present in text")),
      ("median concreteness score present in text", this.wordConcretenessStats("median concreteness score present in text")),
      ("75th %ile concreteness score present in text", this.wordConcretenessStats("75th %ile concreteness score present in text")),
      ("concreteness score of most used noun", this.wordConcretenessStats("concreteness score of most used noun")),
      ("concreteness score of most used verb", this.wordConcretenessStats("concreteness score of most used verb")),
      ("concreteness score of most used adjective", this.wordConcretenessStats("concreteness score of most used adjective"))
    )
  }

  */

}
