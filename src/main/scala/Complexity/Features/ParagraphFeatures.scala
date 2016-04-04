package Complexity.Features

import Complexity.TextDocument
import org.apache.commons.math3.stat.Frequency
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

/**
 * Created by mcapizzi on 8/18/15.
 */

/**
  * Generates features relating to units larger than a sentence
 * @param td [[TextDocument]] for the document to be analyzed
  */
class ParagraphFeatures(val td: TextDocument) {

  //paragraph size stats
  /**
    * Gets lengths (number of sentences) of each paragraph in document
    * @return `Vector` of paragraph lengths
    */
  def getParagraphLengths: Vector[Double] = {
    for (paragraph <- td.paragraphs) yield {
      paragraph.rawSentences.length.toDouble
    }
  }

  /**
    * Generates basic distribution of paragraph lengths
    * @return Values representing distribution of paragraph lengths
    *
    *         {{{
    *           Map(
    *             "minimum paragraph length" -> ?,
    *             "25th %ile paragraph length" -> ?,
    *             "mean paragraph length" -> ?,
    *             "median paragraph length" -> ?,
    *             75th%ile paragraph length" -> ?,
    *             maximum paragraph length" -> ?
    *           )
    *         }}}
    */
  def paragraphLengthStats: Map[String, Double] = {
    val stat = new DescriptiveStatistics()
    this.getParagraphLengths.foreach(stat.addValue)
    Map(
      "minimum paragraph length" -> stat.getMin,
      "25th %ile paragraph length" -> stat.getPercentile(25),
      "mean paragraph length" -> stat.getMean,
      "median paragraph length" -> stat.getPercentile(50),
      "75th %ile paragraph length" -> stat.getPercentile(75),
      "maximum paragraph length" -> stat.getMax
    )
  }

  /**
    * Extracts relation information from [[TextDocument.rawDiscourseParses]]
    * @return `Vector` of `(relation, direction)` where `direction` includes `()` in the string
    */
  def getDiscourseRelations: Vector[Array[(String, String)]] = {
    val allTrees = td.rawDiscourseParses

    //clean discourse parse
    val finalTuple = for (tree <- allTrees) yield {
      val split = tree.toString.split("\n")     //split at "\n"
      val trimmed = for (line <- split) yield {
                      line.trim                   //trim whitespace
                    }
      val filtered = trimmed.filterNot(each =>
                      each.startsWith("TEXT")       //remove text lines
                      )
      val filteredSplit = for (line <- filtered) yield {
        line.split(" ")                               //split relation from direction
      }
      for (item <- filteredSplit) yield {
        (                                               //build output tuple
          item.head,
          if (item.length == 1) "n/a" else item(1)
        )
      }
    }
    finalTuple
  }


  /**
    * Generates percentage of each type of discourse relation
    * @return Values representing distibution amoung discoure relations
    *         
    *         {{{
    *           Map(
    *             "minimum number of relations per paragraph" -> ?,
    *             "25th %ile number of relations per paragraph" -> ?,
    *             "mean number of relations per paragraph" -> ?,
    *             "median number of relations per paragraph" -> ?,
    *             "75th %ile number of relations per paragraph" -> ?,
    *             "maximum number of relations per paragraph" -> ?,
    *             "percent of L->R relations in text" -> ?,
    *             "percent of R->L relations in text" -> ?,
    *             "percent of directionless relations in text" -> ?,
    *             "percent of 'span' relations in text" -> ?,
    *             "percent of 'comparison' relations in text" -> ?,
    *             "percent of 'background' relations in text" -> ?,
    *             "percent of 'textual-organization' relations in text" -> ?,
    *             "percent of 'joint' relations in text" -> ?,
    *             "percent of 'attribution' relations in text" -> ?,
    *             "percent of 'enablement' relations in text" -> ?,
    *             "percent of 'condition' relations in text" -> ?,
    *             "percent of 'temporal' relations in text" -> ?,
    *             "percent of 'explanation' relations in text" -> ?,
    *             "percent of 'cause' relations in text" -> ?,
    *             "percent of 'contrast' relations in text" -> ?,
    *             "percent of 'evaluation' relations in text" -> ?,
    *             "percent of 'topic-change' relations in text" -> ?,
    *             "percent of 'same-unit' relations in text" -> ?,
    *             "percent of 'manner-means' relations in text" -> ?,
    *             "percent of 'summary' relations in text" -> ?,
    *             "percent of 'topic-comment' relations in text" -> ?,
    *             "percent of 'elaboration' relations in text" -> ?
    *           )
    *         }}}
    * @todo Count empty `Array`s which would indicate no relation found
    */
  def discourseRelationsStats: Map[String, Double] = {
    //get paragraph lengths - for normalization
    val paragraphLengths = this.getParagraphLengths

    //extract just a list of relations
    val relations = this.getDiscourseRelations.map(_.map(_._1))
    //extract just a list of directions for relations
    val directions = this.getDiscourseRelations.map(_.map(_._2))

    //build tuples of (paragraph length, list of relations, list of directions)
    val tuple = (paragraphLengths, relations, directions).zipped.toVector

    //call descriptive stats for distribution of relation types
    val relationCountStats = new DescriptiveStatistics()

    //number of relations / number of sentences in paragraph
    val relationCount = for (paragraph <- tuple) yield {
      paragraph._2.length.toDouble / paragraph._1.toDouble
    }

    //add to stats
    relationCount.foreach(relationCountStats.addValue)

    //call frequency for directions
    val directionFreq = new Frequency()
    //add directions to frequency
    tuple.flatMap(_._3).foreach(directionFreq.addValue)


    //call frequency for relations types
    val relationFreq = new Frequency()
    //add relations to frequency
    tuple.flatMap(_._2).foreach(relationFreq.addValue)

    //calculate ratio of each direction (including "n/a")
    val directionRatio = tuple.flatMap(_._3).map(direction =>
                            direction -> directionFreq.getPct(direction)
                          ).distinct.toMap                                        //make map

    //calculate count for each relation type (normalized by length of paragraph)
    val relationTypes = tuple.flatMap(_._2).map(relation =>
                            relation -> relationFreq.getCount(relation) / tuple.flatMap(_._2).length.toDouble
                        ).distinct.toMap

    Map(
      "minimum number of relations per paragraph" -> relationCountStats.getMin,
      "25th %ile number of relations per paragraph" -> relationCountStats.getPercentile(25),
      "mean number of relations per paragraph" -> relationCountStats.getMean,
      "median number of relations per paragraph" -> relationCountStats.getPercentile(50),
      "75th %ile number of relations per paragraph" -> relationCountStats.getPercentile(75),
      "maximum number of relations per paragraph" -> relationCountStats.getMax,
      "percent of L->R relations in text" -> directionRatio.getOrElse("(LeftToRight)", 0.0),
      "percent of R->L relations in text" -> directionRatio.getOrElse("(RightToLeft)", 0.0),
      "percent of directionless relations in text" -> directionRatio.getOrElse("n/a", 0.0),
      "percent of 'span' relations in text" -> relationTypes.getOrElse("span", 0.0),
      "percent of 'comparison' relations in text" -> relationTypes.getOrElse("comparison", 0.0),
      "percent of 'background' relations in text" -> relationTypes.getOrElse("background", 0.0),
      "percent of 'textual-organization' relations in text" -> relationTypes.getOrElse("textual-organization", 0.0),
      "percent of 'joint' relations in text" -> relationTypes.getOrElse("joint", 0.0),
      "percent of 'attribution' relations in text" -> relationTypes.getOrElse("attribution", 0.0),
      "percent of 'enablement' relations in text" -> relationTypes.getOrElse("enablement", 0.0),
      "percent of 'condition' relations in text" -> relationTypes.getOrElse("condition", 0.0),
      "percent of 'temporal' relations in text" -> relationTypes.getOrElse("temporal", 0.0),
      "percent of 'explanation' relations in text" -> relationTypes.getOrElse("explanation", 0.0),
      "percent of 'cause' relations in text" -> relationTypes.getOrElse("cause", 0.0),
      "percent of 'contrast' relations in text" -> relationTypes.getOrElse("contrast", 0.0),
      "percent of 'evaluation' relations in text" -> relationTypes.getOrElse("evaluation", 0.0),
      "percent of 'topic-change' relations in text" -> relationTypes.getOrElse("topic-change", 0.0),
      "percent of 'same-unit' relations in text" -> relationTypes.getOrElse("same-unit", 0.0),
      "percent of 'manner-means' relations in text" -> relationTypes.getOrElse("manner-means", 0.0),
      "percent of 'summary' relations in text" -> relationTypes.getOrElse("summary", 0.0),
      "percent of 'topic-comment' relations in text" -> relationTypes.getOrElse("topic-comment", 0.0),
      "percent of 'elaboration' relations in text" -> relationTypes.getOrElse("elaboration", 0.0)
    )
  }

  
  /**
    * Generates `Vector` of all features for [[TextDocument]] <br>
    *   First two items in `Vector` are `title` and `grade level`
    */
  def makeParagraphFeatureVector: Vector[(String, Double)] = {
    Vector(
      td.title.getOrElse("") -> 0.0,
      td.gradeLevel.getOrElse("") -> 0.0,
      //paragraph length
      "minimum paragraph length" -> this.paragraphLengthStats("minimum paragraph length"),
      "25th %ile paragraph length" -> this.paragraphLengthStats("25th %ile paragraph length"),
      "mean paragraph length" -> this.paragraphLengthStats("mean paragraph length"),
      "median paragraph length" -> this.paragraphLengthStats("median paragraph length"),
      "75th %ile paragraph length" -> this.paragraphLengthStats("75th %ile paragraph length"),
      "maximum paragraph length" -> this.paragraphLengthStats("maximum paragraph length"),
      //distribution of relations per paragraph
      "minimum number of relations per paragraph" -> this.discourseRelationsStats("minimum number of relations per paragraph"),
      "25th %ile number of relations per paragraph" -> this.discourseRelationsStats("25th %ile number of relations per paragraph"),
      "mean number of relations per paragraph" -> this.discourseRelationsStats("mean number of relations per paragraph"),
      "median number of relations per paragraph" -> this.discourseRelationsStats("median number of relations per paragraph"),
      "75th %ile number of relations per paragraph" -> this.discourseRelationsStats("75th %ile number of relations per paragraph"),
      "maximum number of relations per paragraph" -> this.discourseRelationsStats("maximum number of relations per paragraph"),
      //percentage of directions
      "percent of L->R relations in text" -> this.discourseRelationsStats("percent of L->R relations in text"),
      "percent of R->L relations in text" -> this.discourseRelationsStats("percent of R->L relations in text"),
      "percent of directionless relations in text" -> this.discourseRelationsStats("percent of directionless relations in text"),
      //percentage of relations
      "percent of 'span' relations in text" -> this.discourseRelationsStats("percent of 'span' relations in text"),
      "percent of 'comparison' relations in text" -> this.discourseRelationsStats("percent of 'comparison' relations in text"),
      "percent of 'background' relations in text" -> this.discourseRelationsStats("percent of 'background' relations in text"),
      "percent of 'textual-organization' relations in text" -> this.discourseRelationsStats("percent of 'textual-organization' relations in text"),
      "percent of 'joint' relations in text" -> this.discourseRelationsStats("percent of 'joint' relations in text"),
      "percent of 'attribution' relations in text" -> this.discourseRelationsStats("percent of 'attribution' relations in text"),
      "percent of 'enablement' relations in text" -> this.discourseRelationsStats("percent of 'enablement' relations in text"),
      "percent of 'condition' relations in text" -> this.discourseRelationsStats("percent of 'condition' relations in text"),
      "percent of 'temporal' relations in text" -> this.discourseRelationsStats("percent of 'temporal' relations in text"),
      "percent of 'explanation' relations in text" -> this.discourseRelationsStats("percent of 'explanation' relations in text"),
      "percent of 'cause' relations in text" -> this.discourseRelationsStats("percent of 'cause' relations in text"),
      "percent of 'contrast' relations in text" -> this.discourseRelationsStats("percent of 'contrast' relations in text"),
      "percent of 'evaluation' relations in text" -> this.discourseRelationsStats("percent of 'evaluation' relations in text"),
      "percent of 'topic-change' relations in text" -> this.discourseRelationsStats("percent of 'topic-change' relations in text"),
      "percent of 'same-unit' relations in text" -> this.discourseRelationsStats("percent of 'same-unit' relations in text"),
      "percent of 'manner-means' relations in text" -> this.discourseRelationsStats("percent of 'manner-means' relations in text"),
      "percent of 'summary' relations in text" -> this.discourseRelationsStats("percent of 'summary' relations in text"),
      "percent of 'topic-comment' relations in text" -> this.discourseRelationsStats("percent of 'topic-comment' relations in text"),
      "percent of 'elaboration' relations in text" -> this.discourseRelationsStats("percent of 'elaboration' relations in text")
    )
  }


}


