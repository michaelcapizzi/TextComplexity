package Complexity.Features


import Complexity.SupportMethods.Concreteness._
import Complexity.TextDocument
import org.apache.commons.math3.stat.Frequency
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics


/**
 * Created by mcapizzi on 8/17/15.
 */
class LexicalFeatures(
                       val textDocument: TextDocument
                     ) {

  //TODO build statistics about word and ngram frequency
  //TODO how to deal with no words in that part of speech?

  //number of distinct tokens
  def distinctTokens: Double = {
    this.textDocument.getWords.map(
      _.toLowerCase).
      distinct.
      length.toDouble
  }

  //ratio of distinct words
  def distinctTokenRatio: Double = {
    this.distinctTokens / this.textDocument.getWords.length.toDouble
  }

  //word lengths
  def wordLengths(proper: Boolean): Vector[Double] = {
    if (proper == true) {
      this.textDocument.getWords.map(
        _.length.toDouble
      )
    } else {
      this.textDocument.getWordsMinusProperNouns.map(
        _.length.toDouble
      )
    }
  }

  //word length stats
  def wordLengthStats(proper: Boolean): Map[String, Double] = {
    val stat = new DescriptiveStatistics()
    if (proper == true) {
      this.wordLengths(true).foreach(stat.addValue)
      Map(
        "minimum word length" -> stat.getMin,
        "25th %ile word length" -> stat.getPercentile(25),
        "mean word length" -> stat.getMean,
        "median word length" -> stat.getPercentile(50),
        "75th %ile word length" -> stat.getPercentile(75),
        "maximum word length" -> stat.getMax
      )
    } else {
      this.wordLengths(false).foreach(stat.addValue)
      Map(
        "minimum word length" -> stat.getMin,
        "25th %ile word length" -> stat.getPercentile(25),
        "mean word length" -> stat.getMean,
        "median word length" -> stat.getPercentile(50),
        "75th %ile word length" -> stat.getPercentile(75),
        "maximum word length" -> stat.getMax
      )
    }
  }

  //most frequently used words by POS
  def mostFrequentWords: ((String, String), (String, String), (String, String)) = {
    val frequencyNouns = new Frequency()
    val frequencyAdjectives = new Frequency()
    val frequencyVerbs = new Frequency()
    val nouns = textDocument.lexicalTuple.filter(tuple => tuple._2._2.matches("NNS?"))          //filter just nouns
    val adjectives = textDocument.lexicalTuple.filter(tuple => tuple._2._2.matches("JJ.?"))     //filter just adjectives
    val verbs = textDocument.lexicalTuple.filter(tuple => tuple._2._2.matches("VB.*"))          //filter just verbs
    nouns.foreach(word => frequencyNouns.addValue(word._1))                                 //count nouns
    adjectives.foreach(word => frequencyAdjectives.addValue(word._1))                       //count adjectives
    verbs.foreach(word => frequencyVerbs.addValue(word._1))                                 //count verbs
    val mostFreqNoun = nouns.map(tuple =>
        (
          tuple._1,                                                                       //rebuild tuple
          (
            tuple._2._1,
            tuple._2._2,
            tuple._2._3,
            frequencyNouns.getCount(tuple._1).toDouble                                    //adding frequency count
            )
          )
      ).distinct.sortBy(_._2._4).reverse.take(1)                                          //sort by highest frequency and take top one
    val mostFreqAdj = adjectives.map(tuple =>
        (
          tuple._1,                                                                       //rebuild tuple
          (
            tuple._2._1,
            tuple._2._2,
            tuple._2._3,
            frequencyAdjectives.getCount(tuple._1).toDouble                               //adding frequency count
            )
          )
      ).distinct.sortBy(_._2._4).reverse.take(1)                                          //sort by highest frequency and take top one
    val mostFreqVerb = verbs.map(tuple =>
        (
          tuple._1,                                                                       //rebuild tuple
          (
            tuple._2._1,
            tuple._2._2,
            tuple._2._3,
            frequencyVerbs.getCount(tuple._1).toDouble                                    //adding frequency count
            )
          )
      ).distinct.sortBy(_._2._4).reverse.take(1)                                          //sort by highest frequency and take top one
    (                                                                                   //build map of most frequency word for each POS
      "noun" -> mostFreqNoun.head._1,
      "adjective" -> mostFreqAdj.head._1,
      "verb" -> mostFreqVerb.head._2._1                                                 //take lemma of verb
      )
  }

  //# of total distinct lemmas by part of speech
  //verb (VB.*)
  //adjective (JJ.*)
  //conjunctions (CC)
  def countDistinctPOS(pos: String): Double = {
    textDocument.lexicalTuple.
      filter(_._2._2.matches(pos)).                       //take only desired POS - use regex
      map(_._2._1).                                       //extract just the lemmas from tuple
      distinct.length.                                    //count distinct
      toDouble / textDocument.wordCountMinusProperNouns   //normalized over wordCountMinusProperNouns
  }

  //TODO build this method
  //# of distinct word families
  def wordFamilyCount = {
    //stemmer? to detect word families?
    //normalize over wordCountMinusProperNouns
  }

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


}
