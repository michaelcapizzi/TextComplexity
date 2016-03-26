package Complexity.MachineLearning

import Complexity.Features.{ParagraphFeatures, SyntacticFeatures, LexicalFeatures}
import edu.arizona.sista.learning.RVFDatum
import edu.arizona.sista.struct.Counter

/**
 * Created by mcapizzi on 8/30/15.
 */

/*
class FeatureExtractor(
                        textDoc: TextDocument,
                        val lexFeatures: LexicalFeatures = null,
                        val synFeatures: SyntacticFeatures = null,
                        val parFeatures: ParagraphFeatures = null
                        //more to add here as they are built
                      ) {

  //TODO redo to match how its done in PitchVantage

  val metadata = Vector((this.textDoc.title, 0d), (this.textDoc.gradeLevel, 0d))
  val lexFeatureVector = if (lexFeatures != null) lexFeatures.makeLexicalFeatureVector else null
  val synFeatureVector = if (synFeatures != null) synFeatures.makeSyntacticFeatureVector else null
  val parFeatureVector = if (parFeatures != null) parFeatures.makeParagraphFeatureVector else null

  val finalFeatureVector = (metadata ++ lexFeatureVector.slice(2, lexFeatureVector.length) ++ synFeatureVector.slice(2, synFeatureVector.length) ++ parFeatureVector.slice(2, parFeatureVector.length)).filterNot(_ == null)

  val mlDatum = makeMLDatum

  def makeMLDatum: RVFDatum[String, String] = {
    val counter = new Counter[String]
    for (f <- this.finalFeatureVector.slice(2, this.finalFeatureVector.length)) {
      counter.setCount(f._1, f._2)
    }
    new RVFDatum[String, String](this.textDoc.gradeLevel, counter)
  }
}
*/