package Complexity

import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.struct.Counter
import java._

/**
  * Class to house all the NLP elements needed for feature selection later.
  * It is intended to be used for EACH paragraph of the text
  * (because of the computational cost of the Discourse Parser).
  */
class ProcessedParagraph(
                      val text: Option[String],
                      val annotatedDoc: Option[Document],
                      val processor: CoreNLPProcessor,
                      val title: Option[String] = None,
                      val author: Option[String] = None,
                      val paragraphNumber: Option[Int] = None,
                      val chapter: Option[Int] = None,
                      val gradeLevel: Option[String] = None
                    ) {

  //processors Document
  val doc = if (text.nonEmpty) {
              this.processor.mkDocument(this.text.get)
            } else {
              this.annotatedDoc.get
            }

  //annotate document
  def annotate: Unit = {
    this.processor.annotate(this.doc)
    this.doc.clear()                      //clear from memory immediately after annotating
  }



  /////////////////////NLP units//////////////////////

  //raw sentences
  def rawSentences: Vector[edu.arizona.sista.processors.Sentence] = {
    this.doc.sentences.toVector
  }

  //words
  def words(withPunctuation: Boolean): Vector[Vector[String]] = {
    if (withPunctuation) {
      this.doc.sentences.map(_.
        words.toVector
      ).toVector
    } else {
      this.doc.sentences.map(_.
        words.toVector.filter(token =>
          token.matches("[A-z0-9\']+")    //keep only alpha-numeric
          )
      ).toVector
    }
  }

  //lemmas
  def lemmas(withPunctuation: Boolean): Vector[Vector[String]] = {
    if (withPunctuation) {
      this.doc.sentences.map(_.
        lemmas.get.toVector       //get required for Option
      ).toVector
    } else {
      this.doc.sentences.map(_.
        lemmas.get.toVector.filter(token =>   //get required for Option
          token.matches("[A-z0-9\']+")    //keep only alpha-numeric
        )
      ).toVector
    }
  }

  //tags
  def tags(withPunctuation: Boolean): Vector[Vector[String]] = {
    if (withPunctuation) {
      this.doc.sentences.map(_.
        tags.get.toVector       //get required for Option
      ).toVector
    } else {
      this.doc.sentences.map(_.
        tags.get.toVector.filter(token =>   //get required for Option
          token.matches("[A-Z]+")    //keep only alpha-numeric
        )
      ).toVector
    }
  }

  //entities
  def entities(withPunctuation: Boolean): Vector[Vector[String]] = {
    if (withPunctuation) {
      this.doc.sentences.map(_.
        entities.get.toVector       //get required for Option
      ).toVector
    } else {
      //in order to filter out punctuation, must have the word
        //build (word, entity) tuple
      val tuples = for (sentence <- this.doc.sentences) yield {
        sentence.words.toVector.zip(sentence.entities.get.toVector)
      }

      //filter punctuation tokens from each sentence
      val filtered = (for (sentence <- tuples) yield {
        sentence.filter(word =>
          word._1.matches("[A-z0-9\']+"))
      }).toVector

      //drop the word from filtered
      for (sentence <- filtered) yield {
        sentence.map(_._2)
      }

    }
  }

  //lexical tuple
    //separated into sentences
    //(word, (lemma, tag, entity label)
  def lexicalTuple(withPunctuation: Boolean): Vector[Vector[(String, (String, String, String))]] = {
  val tuples = for (sentence <- rawSentences) yield {
      sentence.words.toVector zip
        (
          sentence.lemmas.get.toVector,
          sentence.tags.get.toVector,
          sentence.entities.get.toVector
          ).zipped.toVector
    }

    //whether filtering punctuation
    if (withPunctuation) {
      tuples
    } else {
      for (sentence <- tuples) yield {
        sentence.filter(item =>
          item._1.matches("[A-z\']+")
        )
      }
    }
  }

  /*//proper nouns
  def getProperNouns: Vector[String] = {
    this.lexicalTuple(withPunctuation = false).
      flatten.
      filter(tuple =>
        tuple._2._3 == "PERSON" ||        //is either labeled as person
        tuple._2._3 == "LOCATION"         //or location
      ).map(_._1).                        //keep just the word
      distinct                            //eliminate duplicates
  }*/

  //parse trees = SISTA
  def sistaParseTree: Vector[edu.arizona.sista.struct.Tree] = {
    this.rawSentences.map(sentence =>
      sentence.syntacticTree.get
    )
  }

  //parse trees = CoreNLP
  def coreNLPParseTree: Vector[edu.stanford.nlp.trees.Tree] = {
    for (tree <- this.sistaParseTree) yield {
      edu.stanford.nlp.trees.Tree.valueOf(tree.toString)    //convert tree to string and then use Stanford method, valueOf
    }
  }

  //constituents
  def rawConstituents: Vector[util.Set[edu.stanford.nlp.trees.Constituent]] = {
    this.coreNLPParseTree.map(_.
      constituents)
  }

  //dependencies
  def rawDependencies: Vector[edu.arizona.sista.struct.DirectedGraph[String]] = {
    this.rawSentences.map(_.
      dependencies.get)
  }

  //discourse parse
  def rawDiscourseParse: edu.arizona.sista.discourse.rstparser.DiscourseTree = {
    this.doc.discourseTree.get
  }

  ///////////////////stats////////////////////

  //iterate through sentences one time and build all counters for efficiency
    //returns tuple of Map of counters of differnet types
      //(Counter[String]s, Counter[(String, String)]s)
    //TODO did them all in one iteration, but needed tuple output to handle differnet Type
  def buildCounters: (Map[String, Counter[String]], Map[String, Counter[(String, String)]]) = {
    val tokenCounter = new Counter[String]()
    val lemmaCounter = new Counter[String]()
    val tagCounter = new Counter[String]()
    val properNounCounter = new Counter[String]()
    val tokenTagCounter = new Counter[(String, String)]()
    val lemmaTagCounter = new Counter[(String, String)]()

    for (word <- this.lexicalTuple(withPunctuation = false).flatten) {
      tokenCounter.incrementCount(word._1.toLowerCase)
      lemmaCounter.incrementCount(word._2._1.toLowerCase)
      tagCounter.incrementCount(word._2._2)
      tokenTagCounter.incrementCount(word._1.toLowerCase -> word._2._2)
      lemmaTagCounter.incrementCount(word._2._1.toLowerCase -> word._2._2)
      if (word._2._3 == "PERSON" || word._2._3 == "LOCATION") {
        properNounCounter.incrementCount(word._1)
      }
    }

    (
      Map(
        "tokens" -> tokenCounter,
        "lemmas" -> lemmaCounter,
        "tags" -> tagCounter,
        "proper nouns" -> properNounCounter
      ),
    Map(
        "tokens-tags" -> tokenTagCounter,
        "lemmas-tags" -> lemmaTagCounter
      )
    )

  }



}
