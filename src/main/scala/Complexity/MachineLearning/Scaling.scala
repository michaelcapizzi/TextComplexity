package Complexity.MachineLearning

import edu.arizona.sista.learning.{ScaleRange, Datum, Datasets, RVFDataset}
import edu.arizona.sista.struct.Counter

/**
  * Supporting methods to scale all feature values between 0 and 1
  *
  */
object Scaling {


  /**
    * Scales all datums in a dataset
     * @param dataSet Dataset of datums to be scaled
    * @return `Seq` of feature values, now scaled
    */
  def normalizeData(dataSet: RVFDataset[Int, String]): IndexedSeq[Counter[String]] = {

    //set the scale range
    val normalizedScaleRange = Datasets.svmScaleRVFDataset(dataSet, 0, 1)

    //iterate through dataset
    for (i <- 0 to dataSet.size - 1) yield {
      Datasets.svmScaleDatum(
                            dataSet.mkDatum(i).featuresCounter,
                            normalizedScaleRange,
                            0,
                            1
      )
    }
  }

  /**
    * Scales one individual datum
    * @param datum Datum to be scaled
    * @param range Normalized range `Dataset.svmScaleRVFDataset([dataset], 0, 1)`
    * @return Feature values, now scaled
    */
  def normalizeDatum(datum: Datum[Int, String], range: ScaleRange[String]): Counter[String] = {
    Datasets.svmScaleDatum(datum.featuresCounter, range, 0, 1)
  }


}
