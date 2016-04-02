package Complexity.MachineLearning

/**
 * Contains methods for evaluation
 */
class EvaluationMetrics(
                         scoreList: Vector[(String, String, String)]     //(title, mlScore, actualScore)
                         ) {

  /**
    * Makes a list of the possible labels
    */
  val possibleLabels = scoreList.map(_._3).distinct


  /**
    * Measure of accuracy (correct / total)
    * @param mlScoreList `Vector` of `Tuples` of `(title, predicted label, gold label)`
    * @return Percent correct
    */
  def accuracy(mlScoreList: Vector[(String, String, String)]): Double = {
    //helper function for determining correct score
    def isAccurate(mlScore: String, actualScore: String): Int = {
      if (mlScore == actualScore) 1 else 0
    }


    (
      mlScoreList.map(item =>
        isAccurate(item._2, item._3)
    ).sum.toDouble /                                //sum of all correct items
        mlScoreList.length.toDouble) * 100          //divided by total number of items then multiplied by 100
  }


  //TODO build method to measure distance accuracy
  //def distanceAccuracy


  //TODO build method for accuracy label histogram
  /*def distanceAccuracyLabelHistogram(mlScoreList: Vector[(String, String, String)]) = {
    val distanceAccuracyHistogram = this.distanceAccuracyHistogram(mlScoreList: Vector[(String, String, String)])

  }*/


  //TODO build method for accuracy total histogram
  /*def distanceAccuracyTotalHistogram(mlScoreList: Vector[(String, String, String)], numberOfClasses: Int) = {
    val distanceAccuracyScores = if (numberOfClasses == 6) this.distanceAccuracy6(mlScoreList) else this.distanceAccuracy3(mlScoreList)

    for (distance <- distanceAccuracyScores.map(_._2).distinct.sorted) yield {
      distance -> distanceAccuracyScores.count(_._2 == distance)
    }
  }*/


  /**
    * Generates a label of `true positive`, `true negative`, `false positive`, and `false positive`
    * @param mlScoreList `Vector` of `Tuples` of `(title, predicted label, gold label)`
    */
  def relevanceLabels(mlScoreList: Vector[(String, String, String)]): Map[String, Vector[String]] = {
    //helper function to determine label
    def determineRelevanceLabels(relevantClass: String, mlScore: String, actualScore: String): String = {
      if (relevantClass == actualScore & relevantClass == mlScore) "truePositive"           //it was relevant, and it was correctly scored as relevant
      else if (relevantClass != actualScore & relevantClass == mlScore) "falsePositive"     //it was irrelevant, but it was incorrectly scored as relevant
      else if (relevantClass == actualScore & relevantClass != mlScore) "falseNegative"     //it was relevant, but it was incorrectly scored as irrelevant
      else "trueNegative"
    }

    //iterate through data points
    (for (label <- this.possibleLabels) yield {
      label -> mlScoreList.map(score => determineRelevanceLabels(label, score._2, score._3))      //generate relevance tags for each item
    }).toMap                                                                                        //convert to a Map
  }


  /**
    * Recall - how many actual instances were predicted correctly
    * @param mlScoreList `Vector` of `Tuples` of `(title, predicted label, gold label)`
    * @return `Map` of `(type, recall score)`
    */
  def recall(mlScoreList: Vector[(String, String, String)]): Map[String, Double] = {
    //helper function for calculating recall
    def calculateRecall(truePositive:Double, falseNegative: Double): Double = {
      if ((truePositive + falseNegative) == 0) 0                                            //in case denominator is 0
      else truePositive / (truePositive + falseNegative)                                    //otherwise calculate recall
    }

    //get relevance label map
    val relevanceLabelsMap = relevanceLabels(mlScoreList)

    //iterate through data points
    (for (relevance <- relevanceLabelsMap.keySet.toList) yield {
      (
        relevance,
        calculateRecall(
                        relevanceLabelsMap(relevance).count(_.matches("truePositive")).toDouble,
                        relevanceLabelsMap(relevance).count(_.matches("falseNegative")).toDouble
                        )
      )
    }).toMap      //convert to Map
  }


  /**
    * Precision - how many predicted instances were correct
    * @param mlScoreList `Vector` of `Tuples` of `(title, predicted label, gold label)`
    * @return `Map` of `(type, precision score)`
    */
  def precision(mlScoreList: Vector[(String, String, String)]): Map[String, Double] = {
    //helper function for calculating precision
    def calculatePrecision(truePositive: Double, falsePositive: Double): Double = {
      if ((truePositive + falsePositive) == 0) 0 //in case denominator is 0
      else truePositive / (truePositive + falsePositive) //otherwise calculate recall
    }

    //get relevance label map
    val relevanceLabelsMap = relevanceLabels(mlScoreList)

    //iterate through data points
    (for (relevance <- relevanceLabelsMap.keySet.toList) yield {
      (
        relevance,
        calculatePrecision(
                            relevanceLabelsMap(relevance).count(_.matches("truePositive")).toDouble,
                            relevanceLabelsMap(relevance).count(_.matches("falsePositive")).toDouble
                          )
      )
    }).toMap    //convert to Map
  }

  /**
    * F1 = (2 * precision * recall) / (precision + recall)
    * @param precisionScore As calculated by [[precision]]
    * @param recallScore As calculated by [[recall]]
    */
  def calculateF1(precisionScore: Double, recallScore: Double): Double = {
    if ((precisionScore + recallScore) == 0) 0                                            //in case denominator is 0
    else (2 * precisionScore * recallScore) / (precisionScore + recallScore)              //otherwise calculate recall
  }


  /**
    * Generates F1 score for each type
    * @param mlScoreList `Vector` of `Tuples` of `(title, predicted label, gold label)`
    * @return `Map` of `(type, f1 score)`
    */
  def f1(mlScoreList: Vector[(String, String, String)]): Map[String, Double] = {
    val relevanceLabelsMap = relevanceLabels(mlScoreList)
    (for (relevance <- relevanceLabelsMap.keySet.toList) yield {
      val precisionScore = precision(mlScoreList)(relevance)
      val recallScore = recall(mlScoreList)(relevance)
      relevance -> calculateF1(precisionScore, recallScore)
    }).toMap
  }


  /**
    * Calculates macro scores for `precision`, `recall`, and `f1` <br>
    *   Calculated as the average of score for each type
    * @param mlScoreList `Vector` of `Tuples` of `(title, predicted label, gold label)`
    * @return `Map` of `(type, f1 score)`
    */
  def macroScores(mlScoreList: Vector[(String, String, String)]): Map[String, Double] = {
    val macroPrecision = precision(mlScoreList: Vector[(String, String, String)]).values.toList.sum / possibleLabels.length
    val macroRecall = recall(mlScoreList: Vector[(String, String, String)]).values.toList.sum / possibleLabels.length
    Map(
      "macroPrecision" -> macroPrecision,
      "macroRecall" -> macroRecall,
      "macroF1" -> calculateF1(macroPrecision, macroRecall)
    )
  }

  //NOTE: the description below may be inaccurate...

  //to extract
  //metrics(key) --> will yield you an Any of all three
  //to go deeper
  //metrics(key).asInstanceOf[Map[???]](key)

}
