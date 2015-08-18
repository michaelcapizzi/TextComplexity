/**
 * Created by mcapizzi on 8/17/15.
 */

import java.io._
import edu.arizona.sista.learning.Datum
import edu.arizona.sista.processors.DocumentSerializer
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.struct.Counter
import edu.stanford.nlp.trees.tregex.TregexPattern
import edu.stanford.nlp.trees.{CollinsHeadFinder, MemoryTreebank, DiskTreebank, Tree}
import org.apache.commons.math3.stat.Frequency
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
//import Concreteness._
import scala.collection.mutable

class TextDocument(
                    val text: Vector[String],               //each vector contains a paragraph of original text
                    val processor: CoreNLPProcessor,
                    val author: String = null,
                    val title: String = null,
                    val chapter: String = null,
                    val gradeLevel: String = null

                  ) {

  val textDoc = text.map(processor.mkDocument(_))

  def fullText: String = {
    this.text.mkString(" ")
  }

  ////////////////////////// access Processors  //////////////////////////

  def annotate: Unit = {
    this.textDoc.foreach(processor.annotate)
    this.textDoc.foreach(_.clear())
  }


  ////////////////////////// for normalizing //////////////////////////

  def lexicalTuple: Vector[(String, (String, String, String))] = {
    this.textDoc.map(_.sentences.map(_.words.toVector)).flatten.flatten zip             //the word
      (
        this.textDoc.map(_.sentences.map(_.lemmas.get.toVector)).flatten.flatten,         //the lemma
        this.textDoc.map(_.sentences.map(_.tags.get.toVector)).flatten.flatten,           //the POS tag
        this.textDoc.map(_.sentences.map(_.entities.get.toVector)).flatten.flatten        //the NER label
        ).zipped.toVector
  }

  def lexicalTupleInSentences: Vector[Vector[(String, (String, String, String))]] = {
    for (sentence <- this.textDoc.flatMap(_.sentences)) yield {
      sentence.words.toVector zip
        (
          sentence.lemmas.get.toVector,
          sentence.tags.get.toVector,
          sentence.entities.get.toVector
          ).zipped.toVector
    }
  }

  def getWords: Vector[String] = {
    this.lexicalTuple.
      map(_._1).                                  //get the tokens
      filter(_.matches("[A-Za-z]+"))              //only keep words (not punctuation)
  }

  def getProperNouns: Vector[String] = {
    this.lexicalTuple.
      filter(tuple => (tuple._2._3 == "PERSON" || tuple._2._3 == "LOCATION")        //is either PERSON or LOCATION
      && tuple._1.matches("[A-Z].*")).                                              //and capitalized
      map(_._1).distinct                                                            //take just the word and get a distinct list
  }

  def getWordsMinusProperNouns: Vector[String] = {
    this.lexicalTuple.
      filter(_._1.matches("[A-Za-z]+")).                                            //only keep words (not punctuation)
      filterNot(tuple => (tuple._2._3 == "PERSON" || tuple._2._3 == "LOCATION")     //remove words that are either PERSON or LOCATION
      && tuple._1.matches("[A-Z].*")).                                              //and capitalized
      map(_._1)                                                                     //take just the word
  }

  //# of total words
  def wordCount: Double = {
    this.getWords.length.toDouble
  }

  def properNounCount: Double = {
    this.getProperNouns.length.toDouble
  }

  def wordCountMinusProperNouns: Double = {
    this.getWordsMinusProperNouns.length.toDouble
  }

  def getLemmas: Vector[String] = {
    this.lexicalTuple.
      map(_._2._1).                                 //get the lemmas
      filter(_.matches("[A-Za-z]+"))                //only keep words (not punctuation)
  }

  //# of total lemmas
  def lemmaCount: Double = {
    this.lexicalTuple.
      map(_._2._1).                                 //get the lemmas
      count(_.matches("[A-Za-z]+")).toDouble        //only count words (not punctuation)
  }

  //# of sentences
  def sentenceSize: Double = {
    this.textDoc.map(_.sentences.length).sum.toDouble
  }

  //# of paragraphs
  def paragraphSize: Double = {
    this.textDoc.length.toDouble
  }

}
