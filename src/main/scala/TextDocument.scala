package Complexity


import edu.arizona.sista.processors.{CorefMention, CorefChains}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

import scala.collection.immutable.IndexedSeq


/**
 * Created by mcapizzi on 8/17/15.
 */



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

  //raw coreference chains
    //Note: since all paragraphs are a separate document, there will be no coreference between paragraphs
  def rawCoreference: Vector[CorefChains] = {
    this.textDoc.flatMap(_.coreferenceChains)
  }

  //gives list of coreference chains
    //four numbers in each mention
      //(sentence index, head index, startOffset, endOffset)
  def cleanCoreferenceChains: Vector[Vector[Iterable[CorefMention]]] = {
    this.textDoc.map(_.coreferenceChains.get.getChains.toVector)
  }

  //outputs the lexical items that makeup the coreference chains
  def showChains(collapse: Boolean): IndexedSeq[Vector[Iterable[Array[String]]]] = {
    val output = for (i <- this.cleanCoreferenceChains.indices) yield {
                    for (chain <- this.cleanCoreferenceChains(i)) yield {
                      for (mention <- chain) yield {
                        this.textDoc(i).sentences(mention.sentenceIndex).words.slice(mention.startOffset, mention.endOffset)
                      }
                    }
                  }
    if (collapse) {
      output.map(paragraph => paragraph.map(chain => chain.map(mention => Array(mention.mkString(" ")))))
    } else {
      output
    }
  }

  //TODO incorporate a fourth item => coreference
    //where each token has its corefered term here
      //ex. (he, NNP, O, Michael) or (the, DET, O, the)
  def lexicalTuple: Vector[(String, (String, String, String))] = {
    this.textDoc.flatMap(_.sentences.map(_.words.toVector)).flatten zip               //the word
      (
        this.textDoc.flatMap(_.sentences.map(_.lemmas.get.toVector)).flatten,         //the lemma
        this.textDoc.flatMap(_.sentences.map(_.tags.get.toVector)).flatten,           //the POS tag
        this.textDoc.flatMap(_.sentences.map(_.entities.get.toVector)).flatten        //the NER label
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
