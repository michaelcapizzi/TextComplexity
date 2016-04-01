package Complexity.MachineLearning

import java.io.File

import Complexity.Features.Word2Vec._
import Complexity.Features._
import Complexity.Utils.IO._
import Complexity.MachineLearning.MLutils._
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.struct.Lexicon

/**
  * Generates feature matrices for training sets, with options to select subsets of feature types
  */
object BuildFeatureMatrices {

  /**
    *
    * @param args <br>
    *               args(0) = number of classes to use: `3` or `6`
    *               args(1) = full path to file for exported dataset `.svmLight`
    *               args(2+) = List of feature types to include: `lexical`, `syntactic`, `paragraph`, or `all`
    */
  def main(args: Array[String]) = {

    /**
      * Instance of [[edu.arizona.sista.processors.corenlp.CoreNLPProcessor]]
      */
    val p = new CoreNLPProcessor(withDiscourse = true, maxSentenceLength = 450)


    /**
      * List of all documents to be included
      */
    val allTexts = new File(getClass.getResource("/annotatedText/").getPath).listFiles

    /**
      * Array of [[edu.arizona.sista.learning.RVFDatum]]s <br>
      *   Set as `var` so that parallelization can occur
      */
    var datums = Array[edu.arizona.sista.learning.RVFDatum[Int, String]]()


    /**
      * Feature list being used for feature development
       */
    val featureList = args.slice(2, args.length)

    /**
      * w2vMap loaded one time (or None if not using SyntacticFeatures)
       */
    val w2vMap = if (featureList.contains("syntactic") || featureList.contains("all")) {
                      Some(makeMutableMapDense(
                            w2vPath = "/word2vec_SISTA.txt.gz",
                            take = 500000
                            ))
                } else {
                  None
                }

    /**
      * Variable to capture the lexicon (needed for [[exportToSVM]]
      */
    var lexicon = new Lexicon[String]()


    //iterate through all documents in parallel
    for (doc <- allTexts.par) {

      println("Handling " + doc.getName)

      //make TextDocument
      val td = makeDocumentFromSerial(doc.getName, p)

      println(doc.getName + "-" + td.gradeLevel.getOrElse("NotGiven") + ", converted to class #" + convertLabel(td.gradeLevel.getOrElse("NotGiven"), args(0).toInt) + " out of " + args(0) + " classes including (class #0)")


      /**
        * Initialized variable for [[LexicalFeatures]]
        */
      var lex: Option[LexicalFeatures] = None
      /**
        * Initialized variable for [[SyntacticFeatures]]
        */
      var syn: Option[SyntacticFeatures] = None
      /**
        * Initialized variable for [[ParagraphFeatures]]
        */
      var par: Option[ParagraphFeatures] = None
      /**
        * Initialized variable for [[Word2Vec]]
        */
      var w2v: Option[Word2Vec] = None

      //populate appropriate feature classes
      if (featureList.contains("lexical") || featureList.contains("all")) {
        lex = Some(new LexicalFeatures(td))
      }

      if (featureList.contains("syntactic") || featureList.contains("all")) {

        val w2v = new Word2Vec(
                                w2vFilePath = null,
                                vocabulary = td.forW2V,
                                w2vMasterMap = Some(w2vMap.get)
                              )

        syn = Some(new SyntacticFeatures(td, Some(w2v)))
      }

      if (featureList.contains("paragraph") || featureList.contains("all")) {
        par = Some(new ParagraphFeatures(td))
      }


      /**
        * variable containing the [[FeatureExtractor]] class used to accumulate all features
         */
      val fe = new FeatureExtractor(
                                    td = td,
                                    lexFeatures = lex,
                                    synFeatures = syn,
                                    parFeatures = par,
                                    numClasses = args(0).toInt
                                    )

      println(doc.getName + "-label: " + fe.mlDatum.label.toString)
      for (feature <- fe.mlDatum.featuresCounter.keySet) {
        println(doc.getName + "-" + feature + ": " + fe.mlDatum.featuresCounter.getCount(feature))
      }


      //add datum to list of datums
      datums = datums :+ fe.mlDatum


      //update lexicon
      lexicon = fe.mlLexicon

    }


    println("exporting to SVM")

    /*for (d <- datums) {
      println("label: " + d.label.toString)
      for (f <- d.featuresCounter.keySet) {
        println(f + ": " + d.featuresCounter.getCount(f))
      }
    }*/

    //export to SVM
    exportToSVM(
                  datumSeq = datums.toVector,
                  lexicon = lexicon,
                  outputFileName = args(1)
                )



  }
}
