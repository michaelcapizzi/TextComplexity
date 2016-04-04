package Complexity.SupportMethods

import Complexity.Features.Word2Vec
import breeze.linalg.{SparseVector, DenseVector}
import breeze.numerics._
import edu.arizona.sista.struct.Counter

/**
  * Support methods for calculating similarity of word vectors
  */
object Similarity {


  /**
    * Calculates cosine similarity of two `breeze.linalg.DenseVector`s
    * @param vectorOne First vector
    * @param vectorTwo Second vector
    * @return Cosine similarity score
    */
  def cosSim(vectorOne: breeze.linalg.DenseVector[Double], vectorTwo: breeze.linalg.DenseVector[Double]): Double = {
    val normalized = sqrt(vectorOne dot vectorOne) * sqrt(vectorTwo dot vectorTwo)
    val dotProduct = if (vectorOne.length == 0 || vectorTwo.length == 0) 0 else vectorOne dot vectorTwo
    if (dotProduct == 0) 0 else dotProduct / normalized
  }

  /**
    * Calculates cosine similarity of two `breeze.linalg.SparsVector`s
    * @param vectorOne First vector
    * @param vectorTwo Second vector
    * @return Cosine similarity score
    */
  def cosSim(vectorOne: breeze.linalg.SparseVector[Double], vectorTwo: breeze.linalg.SparseVector[Double]): Double = {
    val normalized = sqrt(vectorOne dot vectorOne) * sqrt(vectorTwo dot vectorTwo)
    val dotProduct = if (vectorOne.length == 0 || vectorTwo.length == 0) 0 else vectorOne dot vectorTwo
    if (dotProduct == 0) 0 else dotProduct / normalized
  }


  /**
    * Calculates distance of two `breeze.linalg.DenseVector`s
    * @param vectorOne First vector
    * @param vectorTwo Second vector
    * @param distance Which distance metric to use: `euclidean` or `manhattan`, defaults to `euclidean`
    * @return Distance score
    */
  def vectorDistance(vectorOne: breeze.linalg.DenseVector[Double], vectorTwo: breeze.linalg.DenseVector[Double], distance: String): Double = {
    if (distance == "euclidean") {
      breeze.linalg.functions.euclideanDistance(vectorOne, vectorTwo)
    } else if (distance == "manhattan") {
      breeze.linalg.functions.manhattanDistance(vectorOne, vectorTwo)
    } else {
      breeze.linalg.functions.euclideanDistance(vectorOne, vectorTwo)
    }
  }


  /**
    * Normalizes a `breeze.linalg.DenseVector`
    * @param v The vector to normalize
    * @return Normalized value
    */
  def normalizeVector(v: breeze.linalg.DenseVector[Double]): Double = {
    sqrt(v dot v)
  }

  /**
    * Normalizes a `breeze.linalg.SparseVector`
    * @param v The vector to normalize
    * @return Normalized value
    */  def normalizeVector(v: breeze.linalg.SparseVector[Double]): Double = {
    sqrt(v dot v)
  }


}
