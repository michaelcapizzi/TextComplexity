package Complexity.MachineLearning

import edu.arizona.sista.learning._
import edu.arizona.sista.struct.Counter

/**
  * Supporting methods to scale all feature values between 0 and 1
  *
  */
object Scaling {


  /**
    * Scales all datums in a dataset
    *
    * @param dataSet Dataset of datums to be scaled
    * @return ([[edu.arizona.sista.learning.ScaleRange[String]], `Seq` of feature values, now scaled)
    */
  def normalizeData(dataSet: RVFDataset[Int, String]): ScaleRange[String]/*(ScaleRange[String], IndexedSeq[Counter[String]])*/ = {

    //set the scale range
    val normalizedScaleRange = Datasets.svmScaleRVFDataset(dataSet, 0, 1)

    /*//iterate through dataset
    val scaledData = for (i <- 0 to dataSet.size - 1) yield {
                            Datasets.svmScaleDatum(
                              dataSet.mkDatum(i).featuresCounter,
                              normalizedScaleRange,
                              0,
                              1
                          )
                      }

    (normalizedScaleRange, scaledData)*/

    normalizedScaleRange

  }

  /**
    * Scales one individual datum
    *
    * @param datum Datum to be scaled
    * @param range Normalized range [[edu.arizona.sista.learning.Datasets.svmScaleRVFDataset]]([dataset], 0, 1)
    * @return Rebuilt datum with same label and scaled [[edu.arizona.sista.learning.RVFDatum.featuresCounter]]
    */
  def normalizeDatum(datum: Datum[Int, String], range: ScaleRange[String]): RVFDatum[Int, String] = {
    new RVFDatum[Int, String](
                              datum.label,
                              Datasets.svmScaleDatum(datum.featuresCounter, range, 0, 1)
    )
  }


}
