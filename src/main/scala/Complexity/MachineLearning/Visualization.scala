package Complexity.MachineLearning

import com.quantifind.charts.highcharts
import com.quantifind.charts.highcharts.Highchart._
import com.quantifind.charts.Highcharts._

/**
 * Created by mcapizzi on 9/6/15.
 */
object Visualization {

  //https://github.com/quantifind/wisp
  //http://api.highcharts.com/highcharts

  //help()

  //lowest chart is first model used


  //TODO update with simpler (one classifier) scoreList
  /*def makeAccuracyPie(scoreList: Vector[(String, Vector[(String, String, String)])]) = {
    val eval = new EvaluationMetrics(scoreList)
    val accuracyScores = scoreList.map(ml => eval.accuracy(ml._2))

    for (ml <- accuracyScores) yield {
      pie(
        List(
          ml,                   //remainder
          100 - ml              //accuracy score
        )
      )
      title("Accuracy (in blue)")
    }
  }

  //lowest chart is first model used
  def makeDistanceHistogram(scoreList: Vector[(String, Vector[(String, String, String)])], numberOfClasses: Int) = {
    val eval = new EvaluationMetrics(scoreList)
    val distances = if (numberOfClasses == 6) scoreList.map(ml => eval.distanceAccuracy6(ml._2).map(_._2)) else scoreList.map(ml => eval.distanceAccuracy3(ml._2).map(_._2))

    for (ml <- distances) yield {
      histogram(ml, ml.distinct.length - 1)
      title("Distance from correct label")
    }
  }

  //TODO figure out why legend isn't working
  def makePRF1Chart(scoreList: Vector[(String, Vector[(String, String, String)])]) = {
    val eval = new EvaluationMetrics(scoreList)
    val macroScores = scoreList.map(ml => eval.macroScores(ml._2))

    column(macroScores.map(each => each("macroPrecision")))
    hold
    column(macroScores.map(each => each("macroRecall")))
    hold
    column(macroScores.map(each => each("macroF1")))
    legend(List("precision", "recall", "F1"))
    xAxisType("category")
    xAxis("ML model")
    yAxis("Scores")
    unhold
  }*/


  //graph type
  //title("blah")
  //xAxis("blah")
  //yAxis("blah")
  //legend(List("blah", "blah"))

  //accuracy pie chart
  //pie(List(accuracy, 1 - accuracy))

  //histogram
  //histogram(distances, distances.distinct.length-1)

  //column chart
  //column(List([all precision scores])
  //hold
  //column(List([all recall scores])
  //hold
  //column(List([all f1 scores])
  //xAxisType("category")
  //xAxis("model")
  //yAxis("score")
  //legend(List("precision", "recall", "f1"))

  //how to change 0, 1 to model name?


}