package Complexity

import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.struct.Counter
import java._

/**
  * Class to house all the NLP elements needed for feature selection later.
  * It is intended to be used for '''each''' paragraph of the text
  * (because of the computational cost of `edu.arizona.sista.discourse.rstparser` DiscourseParser.
  * Will be fed to [[TextDocument]] as a Vector of paragraphs
  * @constructor Will represent the paragraph in terms of NLP pipeline
  * @param text If not loading from annotation, the plain text
  * @param annotatedDoc If loading from annotation, the annotated doc
  * @param processor Instance of `edu.arizona.sista.processors.corenlp.CoreNLPProcessor` with the following settings: `(withDiscourse=true, maxSentenceLength = 450)`
  * @param title Optional title of document of which this paragraph is a part
  * @param author Optional author of document of which this paragraph is a part
  * @param paragraphNumber Optional paragraph index in larger document
  * @param chapter Optional chapter of document of which this paragraph is a part
  * @param gradeLevel Optional grade level of the document of which this paragraph is a part
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

  /**
    * `edu.arizona.sista.processors.Document` housing all annotation information
     */
  val doc = if (text.nonEmpty) {
              this.processor.mkDocument(this.text.get)
            } else {
              this.annotatedDoc.get
            }

  /**
    * Annotates the document and then clears from memory
    */
  def annotate: Unit = {
    this.processor.annotate(this.doc)
    this.doc.clear()                      //clear from memory immediately after annotating
  }



  /////////////////////NLP units//////////////////////

  /**
     * @return Vector of `edu.arizona.sista.processors.Sentences`
    */
  def rawSentences: Vector[edu.arizona.sista.processors.Sentence] = {
    this.doc.sentences.toVector
  }

  /**
    *
     * @param withPunctuation Use of `true` will include punctuation as tokens; `false` will remove all punctuation from tokens
    * @return First `Vector` breaks at sentences <br>
    *           Second `Vector` breaks at words.
    */
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

  /**
     * @param withPunctuation Use of `true` will include punctuation as tokens; `false` will remove all punctuation from tokens
    * @return First `Vector` breaks at sentences <br>
    *           Second `Vector` breaks at lemmas.
    */
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

  /**
    * @param withPunctuation Use of `true` will include punctuation as tokens; `false` will remove all punctuation from tokens
    * @return First `Vector` breaks at sentences <br>
    *           Second `Vector` breaks at tags.
    */
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

  /**
     * @param withPunctuation Use of `true` will include punctuation as tokens; `false` will remove all punctuation from tokens
    * @return First `Vector` breaks at sentences <br>
    *         Second `Vector` breaks at tags.
    */
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

  /**
    * Generates a tuple of `(word, (lemma, tag, named entity))`
    * @param withPunctuation Use of `true` will include punctuation as tokens; `false` will remove all punctuation from tokens
    * @return First `Vector` breaks at sentences <br>
    *           Second `Vector` breaks at token. <br>
    *         - `._1` = word <br>
    *         - `._2._1` = lemma <br>
    *         - `._2._2` = tag <br>
    *         - `._2._3` = named entity (`O` for none) <br>
    */
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

  /**
    * SISTA `processors`-generated trees
     * @return Vector of `edu.arizona.sista.struct.Tree`s
    */
  def sistaParseTree: Vector[edu.arizona.sista.struct.Tree] = {
    this.rawSentences.map(sentence =>
      sentence.syntacticTree.get
    )
  }

  /**
    * CoreNLP-generated `Tree`s
     * @return Vector of `edu.stanford.nlp.trees.Tree`s
    *  @see [[http://nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/trees/Tree.html]]
    */
  def coreNLPParseTree: Vector[edu.stanford.nlp.trees.Tree] = {
    for (tree <- this.sistaParseTree) yield {
      edu.stanford.nlp.trees.Tree.valueOf(tree.toString)    //convert tree to string and then use Stanford method, valueOf
    }
  }

  /**
    * CoreNLP-generated `Constituent`s
     * @return `Vector` of `java.util.Set`s of `edu.stanford.nlp.trees.Constituent`s
    *  @see [[http://nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/trees/Constituent.html]]
    */
  def rawConstituents: Vector[util.Set[edu.stanford.nlp.trees.Constituent]] = {
    this.coreNLPParseTree.map(_.
      constituents)
  }

  /** Dependency parse represented in a `pretty print` form of a `directed graph`
    * @example "Frog was in his garden" <br>
    *
    *          ```
    *          edu.arizona.sista.struct.DirectedGraph[String] = <br>
    *          roots: 1 <br>
    *          outgoing: <br>
    *          &nbsp;&nbsp;&nbsp; 0: <br>
    *          &nbsp;&nbsp;&nbsp; 1: (0,nsubj) (4,prep_in) <br>
    *          &nbsp;&nbsp;&nbsp; 2: <br>
    *          &nbsp;&nbsp;&nbsp; 3: <br>
    *          &nbsp;&nbsp;&nbsp; 4: (3,poss) <br>
    *          incoming: <br>
    *          &nbsp;&nbsp;&nbsp; 0: (1,nsubj) <br>
    *          &nbsp;&nbsp;&nbsp; 1: <br>
    *          &nbsp;&nbsp;&nbsp; 2: <br>
    *          &nbsp;&nbsp;&nbsp; 3: (4,poss) <br>
    *          &nbsp;&nbsp;&nbsp; 4: (1,prep_in) <br>
    *          ```
    * @return `Vector` of dependencies represented as a directed graph
    */
  def rawDependencies: Vector[edu.arizona.sista.struct.DirectedGraph[String]] = {
    this.rawSentences.map(_.
      dependencies.get)
  }

  /**
    * @example "Frog was in his garden. Toad came walking by." <br>
    *            ```
    *             edu.arizona.sista.discourse.rstparser.DiscourseTree = <br>
    *               &nbsp;&nbsp;&nbsp; elaboration (LeftToRight) <br>
    *               &nbsp;&nbsp;&nbsp; TEXT:Frog was in his garden . <br>
    *               &nbsp;&nbsp;&nbsp; TEXT:Toad came walking by .
    *            ```
    */
  def rawDiscourseParse: edu.arizona.sista.discourse.rstparser.DiscourseTree = {
    this.doc.discourseTree.get
  }

  ///////////////////stats////////////////////

  /**
    * Builds all `sista.edu.struct.Counter`s in one iteration of the tokens
    * @return `Counter[String]` = `tokens`, `lemmas`, `tags`, `proper nouns` <br>
    *          `Counter[(String, String)]` = `tokens-tags`, `lemmas-tags`
    */
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
