package Complexity.MachineLearning

import scala.util.Random._

/**
  * Contains method for determining statistical sinificance using bootstrap resampling
  * @param evalMetrics Results to be evaluated for statistical significance
  * @todo Not currently functional; needs to be updated
  */
class StatisticalSignificance (evalMetrics: EvaluationMetrics) {


  /*def bootstrapResample(scoreList1: List[(String, String, String)], scoreList2: List[(String, String, String)]): Double = {
    val comparisonList = scoreList1.map(_._1) zip (scoreList1.map(_._2), scoreList2.map(_._2), scoreList1.map(_._3)).zipped.toList
    val f1Scores = getComparisonF1(makeRandomSample(comparisonList))
    f1Scores.map(each => each._1 - each._2).count(item => item > 0).toDouble / f1Scores.length.toDouble

    def makeRandomSample(comparisonScoreList: List[(String, (String, String, String))]): List[List[(String, (String, String, String))]] = {
      val newScoreList = scala.collection.mutable.MutableList[List[(String, (String, String, String))]]()
      for (i <- 1 to 10000) {
        val newSamples = scala.collection.mutable.MutableList[(String, (String, String, String))]()
        for (j <- comparisonScoreList) {
          newSamples += shuffle(comparisonScoreList).head
        }
        newScoreList += newSamples.toList
      }
      newScoreList.toList
    }

    def getComparisonF1(randomComparisonScoreLists: List[List[(String, (String, String, String))]]): List[(Double, Double)] = {
      randomComparisonScoreLists.map(_.map(item => (item._1, item._2._1, item._2._3))).map(     //makes correct mlScoreList
        thing => this.evalMetrics(thing)).map(row => row("Macro")).map(                        //gets to Macro Map
          _.asInstanceOf[Map[String, Double]]).map(each => each("macroF1")) zip                 //gets macroF1s   //zips
        randomComparisonScoreLists.map(_.map(item => (item._1, item._2._2, item._2._3))).map(     //makes correct mlScoreList
          thing => this.evalMetrics(thing)).map(row => row("Macro")).map(                        //gets to Macro Map
            _.asInstanceOf[Map[String, Double]]).map(each => each("macroF1"))                     //gets macroF1s
    }

  }*/

}
