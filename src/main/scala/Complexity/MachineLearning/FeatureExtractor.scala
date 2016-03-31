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
  * @param lexFeatures [[Complexity.Features.LexicalFeatures]] for [[TextDocument]]
  * @param synFeatures [[Complexity.Features.SyntacticFeatures]] for [[TextDocument]]
  * @param parFeatures [[Complexity.Features.ParagraphFeatures]] for [[TextDocument]]
  * @param numClasses Number of classes for classification
  */
class FeatureExtractor(
                        val td: TextDocument,
                        val lexFeatures: Option[LexicalFeatures] = None,
                        val synFeatures: Option[SyntacticFeatures] = None,
                        val parFeatures: Option[ParagraphFeatures] = None,
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
    * `Vector` of features from [[Complexity.Features.LexicalFeatures]]
    */
  val lexFeatureVector = if (lexFeatures.nonEmpty) {
                            Option[Vector[(String, Double)]](
                                lexFeatures.get.makeLexicalFeatureVector.
                                drop(1)                               //drop metadata from vector
                            )
                          } else {
                            None
                          }

  /**
    * `Vector` of features from [[Complexity.Features.SyntacticFeatures]]
    */
  val synFeatureVector = if (synFeatures.nonEmpty) {
                            Option[Vector[(String, Double)]](
                              synFeatures.get.makeSyntacticFeatureVector.
                              drop(1)                               //drop metadata from vector
                            )
                          } else {
                            None
                          }

  /**
    * `Vector` of features from [[Complexity.Features.ParagraphFeatures]]
    */
  val parFeatureVector = if (parFeatures.nonEmpty) {
                            Option[Vector[(String, Double)]](
                              parFeatures.get.makeParagraphFeatureVector.
                              drop(1)                               //drop metadata from vector
                            )
                          } else {
                            None
                          }

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
    * Output of [[edu.arizona.sista.learning.RVFDatum]] and [[edu.arizona.sista.struct.Lexicon]] in same pass
    */
  val (mlDatum, mlLexicon) = makeDatumAndLexicon


  /**
    * Generates a [[edu.arizona.sista.learning.RVFDatum]] and [[edu.arizona.sista.struct.Lexicon]] in same pass of features <br>
    *   `Datum` will be added to the [[edu.arizona.sista.learning.RVFDataset]] <br>
    *     `Lexicon` will be made available in case [[edu.arizona.sista.learning.RVFDataset]] is written to file in [[Complexity.MachineLearning.MLutils.exportToSVM()]]
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
