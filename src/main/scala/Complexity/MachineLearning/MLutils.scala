package Complexity.MachineLearning

import java.io.{PrintWriter, File}
import edu.arizona.sista.learning.{RVFDatum, RVFDataset}
import edu.arizona.sista.struct.Lexicon

/**
  * Supporting methods to accompany machine learning process
  */
object MLutils {

  /**
    * SVMlight format requires numeric labels, so `gradeLevel` must be converted to numbers
    *
    * @param label The given label to convert
    * @param numClasses Number of classes being used
    * @return The label in numeric form
    */
  def convertLabel(label: String, numClasses: Int): Int = {

    if (numClasses == 6) {
      label match {
        case "0001" => 0
        case "0203" => 1
        case "0405" => 2
        case "0608" => 3
        case "0910" => 4
        case "1112" => 5
        case _ => 99
      }
    } else {
      label match {
        case ("0001" | "0203" | "0405") => 0
        case "0608" => 1
        case ("0910" | "1112") => 2
        case _ => 99
      }
    }
  }

  /**
    * SVMlight format requires numeric labels, so `gradeLevel` must be recovered from numbers
    *
    * @param label The given label to recover
    * @param numClasses Number of classes being used
    * @return The label in original string form
    */
  def revertLabel(label: Int, numClasses: Int): String = {
    if (numClasses== 6) {
      label match {
        case 0 => "0001"
        case 1 => "0203"
        case 2 => "0405"
        case 3 => "0608"
        case 4 => "0910"
        case 5 => "1112"
        case 99 => "NoLabelGiven"
      }
    } else {
      label match {
        case 0 => "0005"
        case 1 => "0608"
        case 2 => "0912"
        case 99 => "NoLabelGiven"
      }
    }
  }


  /**
    * Writes dataset to SVMlight format
    * @param datumSeq `Vector` of `Datum`s, made up of [[FeatureExtractor.mlDatum]]s
    * @param lexicon `Lexicon` of feature names, from any [[FeatureExtractor.mlLexicon]]
    * @param outputFileName Location of file to be written
    */
  def exportToSVM(datumSeq: Vector[RVFDatum[Int, String]], lexicon: Lexicon[String], outputFileName: String): Unit = {
    RVFDataset.saveToSvmLightFormat(datumSeq, lexicon, outputFileName)
  }


  /**
    * Imports a dataset from SVMlight format
    * @param fileName Location of file to be imported
    * @return Dataset for use in [[Model]]
    */
  def importFromSVM(fileName: String): RVFDataset[Int, String] = {
    RVFDataset.mkDatasetFromSvmLightFormat(fileName)
  }


}
