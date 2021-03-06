package Complexity

import java.io.File
import Complexity.Features.{Word2Vec, ParagraphFeatures, SyntacticFeatures, LexicalFeatures}
import Complexity.Features.Word2Vec._
import Complexity.MachineLearning.FeatureExtractor
import Complexity.MachineLearning.MLmodel
import Complexity.Utils.IO._
import Complexity.MachineLearning.MLutils._
import Complexity.MachineLearning.Scaling._
import edu.arizona.sista.learning.RVFDatum
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

//TODO predicting the exact same label and confidences for every text file --> the features are being gathered correctly.  It's happening in the test stage

object Predict {

  /**
    * @param args <br>
    *               args(0) = plain text file to be analyzed and classified
    *               args(1) = number of classes to use: `3` or `6`
    *               args(2) = model to use
    *               args(3) = full path to dataset used in training model
    *               args(4+) = List of feature types to include: `lexical`, `syntactic`, `paragraph`, or `all`
    */
  def main(args: Array[String]) = {

    val fileName = args(0)

    /**
      * Feature list being used for feature development
      */
    val featureList = args.slice(4, args.length)


    /**
      * Dataset present only to get [[edu.arizona.sista.learning.ScaleRange]]
      */
    val dataset = importFromSVM(args(3))


    /**
      * Instance of [[edu.arizona.sista.processors.corenlp.CoreNLPProcessor]] with or without Discourse as needed
      */
    val p = if (featureList.contains("paragraph") || featureList.contains("all")) {
                new CoreNLPProcessor(withDiscourse = true, maxSentenceLength = 450)
              } else {
                new CoreNLPProcessor(withDiscourse = false, maxSentenceLength = 450)
              }


    /**
      * The raw text imported from file
       */
    val rawText = importText(fileName)


    //warning message about only one paragraph
    if (
      rawText.length == 1 &&    //only one paragraph
        (
          featureList.contains("paragraph") || featureList.contains("all")  //if using paragraph features
        )
      ){
      println()
      println("Either this document is only one paragraph in length, or paragraphs have not been delimited by a blank line.  Processing time may significantly increase because of the need to run the Discourse Parser over large amounts of continuous text.  This can possibly be shortened by delimiting paragraphs with a blank line.")
      println()
      println()
    }


    /**
      * w2vMap loaded only if using SyntacticFeatures
      */
    val w2vMap = if (featureList.contains("syntactic") || featureList.contains("all")) {
                    println("Loading Word2Vec data")
                    Some(makeMutableMapDense(
                        w2vPath = "/word2vec_SISTA.txt.gz",
                        take = 500000
                    ))
                } else {
                    None
                }


    /**
      * [[Complexity.ProcessedParagraph]]s for each paragraph of raw text
      */
    val procPars = for (paragraph <- rawText) yield {
                      new ProcessedParagraph(
                                      text = Some(paragraph),
                                      annotatedDoc = None,
                                      processor = p
                      )
    }


    //annotate all paragraphs
    procPars.foreach(_.annotate)


    /**
      * [[Complexity.TextDocument]] representing the original document
      */
    val td = new TextDocument(procPars)


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
                                  numClasses = args(1).toInt
                                  )


    /**
      * Feature counter before scaling datum
      */
    val testFeatures = fe.mlDatum.featuresCounter


    /**
      * model to use
      */
    val m = new MLmodel(
                        dataset = dataset,
                        classifierType = args(2)
                        )


    /**
      * Needed for properly scaling prediction datum
      */
    val scaleRange = m.normalizeDataset


    //train the model
    m.train


    /**
      * Converted datum (so that indices match the indices of imported dataset
      */
    val matchingDatum = reformatDatum(fe.mlDatum, fe.mlLexicon)


    /**
      * Scaled version of original datum
      */
    val scaledDatum = normalizeDatum(
                                      datum = matchingDatum,
                                      range = scaleRange
                                    )


    /**
      * Variable to house prediction results
      */
    val prediction = m.predict(
//                              datum = matchingDatum,
                              datum = scaledDatum,
                              numClasses = args(1).toInt
                              )


    //print results
    println("Here are the original feature values:")
    testFeatures.keySet.foreach(f => println(f + ":" + testFeatures.getCount(f)))
    println()
    println("This text is predicted to be of class " + prediction._1 + " with a confidence of " + prediction._2.getCount(prediction._1).toString.take(5) + ".")
    println("Here are the confidences for the other classes:")
    prediction._2.keySet.filterNot(_ == prediction._1).foreach(each =>
                                                                      println(each + ": " + prediction._2.getCount(each).toString.take(5))
                                                              )
    println()
  }
}
