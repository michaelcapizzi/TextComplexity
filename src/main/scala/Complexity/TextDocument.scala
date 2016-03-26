package Complexity

import edu.arizona.sista.struct.Counter

/**
  * Created by mcapizzi on 3/25/16.
  */
class TextDocument (
                    val paragraphs: Vector[ProcessedParagraph]
                   ){

  //parallelized vector of paragraphs
  val parParagraphs = this.paragraphs.par

  //annotate all paragraphs
    //parallelizing causes error in discourse parse - why?
  def annotateAll: Unit = {
    this.paragraphs.foreach(_.annotate)
  }

  //wrappers for fold functions (to accumulate results across all paragraphs)
    //function ([T], [T]) => [T] + [T])

  //fold function for integers
  def foldApplyInt(list: scala.collection.parallel.ParSeq[Int], startingInt: Int, function: (Int, Int) => Int): Int = {
    list.foldLeft(startingInt)(function)
  }

  //fold function for doubles
  def foldApplyDouble(list: scala.collection.parallel.ParSeq[Double], startingDouble: Double, function: (Double, Double) => Double): Double = {
    list.foldLeft(startingDouble)(function)
  }

  //fold function for counters
  def foldApplyCounter(list: scala.collection.parallel.ParSeq[Counter[String]], startingCounter: Counter[String], function: (Counter[String], Counter[String]) => Counter[String]): Counter[String] = {
    list.foldLeft(startingCounter)(function)
  }








}
