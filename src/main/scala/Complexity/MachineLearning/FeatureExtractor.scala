package Complexity.MachineLearning

import Complexity.Features.{ParagraphFeatures, SyntacticFeatures, LexicalFeatures}
import Complexity.TextDocument
import edu.arizona.sista.learning.RVFDatum
import edu.arizona.sista.struct.Counter

/**
 * Created by mcapizzi on 8/30/15.
 */

/**
  * Accumulates features from all of the feature classes
  *
  * @param td [[TextDocument]] of interest
  * @param lexFeatures [[LexicalFeatures]] for [[TextDocument]]
  * @param synFeatures [[SyntacticFeatures]] for [[TextDocument]]
  * @param parFeatures [[ParagraphFeatures]] for [[TextDocument]]
  */
class FeatureExtractor(
                        td: TextDocument,
                        val lexFeatures: LexicalFeatures,
                        val synFeatures: SyntacticFeatures,
                        val parFeatures: ParagraphFeatures
                      ) {

  //TODO redo to match how its done in PitchVantage

  //TODO scale!!!!!

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

  val mlDatum = makeMLDatum

  /**
    * Builds a `edu.arizona.sista.learning.RVFDatum` that will contribute the training dataset <br>
    *   A `datum` consists of a label and a `edu.arizona.sista.struct.Counter` of feature values
    * @return label and the `Counter` holding the feature values
    */
  def makeMLDatum: RVFDatum[String, String] = {
    //implement a Counter
    val counter = new Counter[String]

    //iterate through feature vector
    for (f <- this.finalFeatureVector.slice(2, this.finalFeatureVector.length)) {
      //set the counter to (feature name, feature value)
      counter.setCount(f._1, f._2)
    }
    //return the datum
    new RVFDatum[String, String](
      this.td.gradeLevel.getOrElse("LabelNotProvided"),
      counter
    )

  }
}
