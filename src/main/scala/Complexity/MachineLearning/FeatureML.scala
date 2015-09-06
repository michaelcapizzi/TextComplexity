package Complexity.MachineLearning

import edu.arizona.sista.learning._

/**
 * Created by mcapizzi on 9/6/15.
 */
class FeatureML(
            val classifier: Classifier[String, String]
           ) {

  //the empty dataset
  val dataset = new RVFDataset[String, String]()

  //load model for classifier


  //build dataset from datums
  def makeDataset(datumSeq: Vector[Datum[String, String]], normalize: Boolean): ScaleRange[String] = {
    if (normalize == false) {
      datumSeq.foreach(this.dataset.+=)
      null
    } else {
      datumSeq.foreach(this.dataset.+=)
      Datasets.svmScaleRVFDataset(this.dataset, 0, 1)
    }
  }

  //train classifier
  def train: Unit = {
    this.classifier.train(this.dataset)
  }

  //test dataset
    //input => Vector[titles], Vector[Datum]
    //returns => Vector[title, mlScore, actualScore]
  def test(titles: Vector[String], datumSeq: Vector[Datum[String, String]], normalize: Boolean, range: ScaleRange[String]): Vector[(String, String, String)] = {
    if (normalize == false) {

      (
        titles,                                           //title
        datumSeq.map(d => this.classifier.classOf(d)),    //mlScore
        datumSeq.map(d => d.label)                        //actualScore
      ).zipped.toVector

    } else {
      val normalizedCounters = datumSeq.map(d => Datasets.svmScaleDatum(d.featuresCounter, range))
      val normalizedDatums = for (d <- datumSeq.zip(normalizedCounters)) yield {
                              new RVFDatum[String, String](d._1.label, d._2)
                              }

      (
        titles,                                                   //title
        normalizedDatums.map(d => this.classifier.classOf(d)),    //mlScore
        normalizedDatums.map(d => d.label)                        //actualScore
        ).zipped.toVector

    }
  }

  //save classifier model to file
  def saveClassifier(modelFilepath: String): Unit = {
    this.classifier.saveTo(modelFilepath)
  }

  //load classifier model from file
  def loadClassifier(classifierType: String, modelFilepath: String): Classifier = {
    if (classifierType == "randomForest") {
      RandomForestClassifier.loadFrom[String, String](modelFilepath)
    }
    //TODO add more models later if necessary
    else {
      RandomForestClassifier.loadFrom[String, String](modelFilepath)
    }
  }

  // not yet implemented
  /*//load classifier model from relative path (for remote use)
  def loadClassifier(classifierType: String, reader: java.io.Reader): Classifier = {
    if (classifierType == "randomForest") {
      RandomForestClassifier.loadFrom[String, String](reader)
    }
    //TODO add more models later if necessary
    else {
      RandomForestClassifier.loadFrom[String, String](reader)
    }
  }*/

}

object FeatureML {

  def leaveOneOut()
}


