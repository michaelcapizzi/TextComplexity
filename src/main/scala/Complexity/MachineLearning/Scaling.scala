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
    val normalizedScaleRange = Datasets.svmScaleRVFDataset(dataSet, -1, 1)

    //return the range
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
                              Datasets.svmScaleDatum(datum.featuresCounter, range, -1, 1)
//                              Datasets.svmScaleDatum(datum.featuresCounter, range)
    )
  }


}
