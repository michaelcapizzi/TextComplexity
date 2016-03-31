package Complexity.MachineLearning

import Complexity.Features.{ParagraphFeatures, SyntacticFeatures, LexicalFeatures}
import Complexity.TextDocument
import edu.arizona.sista.learning.RVFDatum
import edu.arizona.sista.struct.{Lexicon, Counter}
import MLutils._

/**
  * Accumulates features from all of the feature classes
  *
  * @param td [[TextDocument]] of interest
  * @param lexFeatures [[LexicalFeatures]] for [[TextDocument]]
  * @param synFeatures [[SyntacticFeatures]] for [[TextDocument]]
  * @param parFeatures [[ParagraphFeatures]] for [[TextDocument]]
  * @param numClasses Number of classes for classification
  */
class FeatureExtractor(
                        val td: TextDocument,
                        val lexFeatures: LexicalFeatures,
                        val synFeatures: SyntacticFeatures,
                        val parFeatures: ParagraphFeatures,
                        val numClasses: Int
                      ) {


  /**
    * Metadata for [[TextDocument]] including `title` and `grade level`
    */
  val metadata = Option[Vector[(String, Double)]](
                  Vector(
                    this.td.title.getOrElse("") -> 0d,
                    this.td.gradeLevel.getOrElse("") -> 0d
                  )
                )

  /**
    * `Vector` of features from [[LexicalFeatures]]
    */
//  val lexFeatureVector = if (lexFeatures != null) lexFeatures.makeLexicalFeatureVector else null
  val lexFeatureVector = Option[Vector[(String, Double)]](
                          lexFeatures.makeLexicalFeatureVector.
                            drop(1)                               //drop metadata from vector
                          )

  /**
    * `Vector` of features from [[SyntacticFeatures]]
    */
//  val synFeatureVector = if (synFeatures != null) synFeatures.makeSyntacticFeatureVector else null
  val synFeatureVector = Option[Vector[(String, Double)]](
                          synFeatures.makeSyntacticFeatureVector.
                            drop(1)                               //drop metadata from vector
                          )

  /**
    * `Vector` of features from [[ParagraphFeatures]]
    */
//  val parFeatureVector = if (parFeatures != null) parFeatures.makeParagraphFeatureVector else null
  val parFeatureVector = Option[Vector[(String, Double)]](
                          parFeatures.makeParagraphFeatureVector.
                            drop(1)                               //drop metadata from vector
  )

  /**
    * Accumulated feature vector of all features
    */
  val finalFeatureVector = (
        metadata ++
        lexFeatureVector ++
        synFeatureVector ++
        parFeatureVector
      ).toVector.flatten


  /**
    * Title of document
    */
  val title = this.finalFeatureVector.head._1


  /**
    * Output of the `Datum` and `Lexicon` in same pass
    */
  val (mlDatum, mlLexicon) = makeDatumAndLexicon


  /**
    * Generates a `Datum` and `Lexicon` in same pass of features <br>
    *   `Datum` will be added to the `Dataset` <br>
    *     `Lexicon` will be made available in case `Dataset` is written to file in [Utils.toSVM]
    * @return Tuple of `(Datum, Lexicon)`
    */
  def makeDatumAndLexicon: (RVFDatum[Int, String], Lexicon[String]) = {
    //implement a Counter
    val counter = new Counter[String]()

    //implement a Lexicon
    val lexicon = new Lexicon[String]()

    //iterate through feature vector
    for (f <- this.finalFeatureVector.slice(2, this.finalFeatureVector.length)) {

      //add to lexicon
      lexicon.add(f._1)

      //set the counter to (feature name, feature value)
      counter.setCount(
                        f._1,
                        f._2
                      )
    }

    //return the datum and lexicon
    (
      new RVFDatum[Int, String](
          convertLabel(this.td.gradeLevel.getOrElse("NoLabelGiven"), this.numClasses),
          counter
          ),
      lexicon
    )
  }

}
