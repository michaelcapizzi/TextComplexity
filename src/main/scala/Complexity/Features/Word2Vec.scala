package Complexity.Features

import breeze.linalg._
import Complexity.SupportMethods.Similarity._
import nak.cluster._
import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.mutable.ParArray
import Complexity.Utils.Compress._


import scala.util.control.Breaks._


/**
  * Builds a class to efficiently handle lookup of Word2Vec vectors. <br>
  *   Builds a specific `Map` of words used in the [[Complexity.TextDocument]] and clusters them for faster lookup
  * @param w2vFilePath Path to Word2Vec file found in [[resources/]]
  * @param vocabulary List of distinct lemmatized tokens, all lowercase to be added to Word2Vec dictionary, use [[Complexity.TextDocument.forW2V]]
  * @param w2vMasterMap Optional pre-loaded `Map` of the files generated from the `.txt` file located at [[w2vFilePath]]
  */
class Word2Vec(
                w2vFilePath: String,
                val vocabulary: scala.collection.immutable.Vector[String],
                val w2vMasterMap: Option[collection.mutable.Map[String, breeze.linalg.DenseVector[Double]]] = None
              ) {

  //used to time loading
  val start = System.currentTimeMillis().toDouble

  /////////////global parameter/////////////////

  //sets the minimum vocabulary size for requiring clustering (to speed up smaller documents by not requiring clustering)
  /**
    * Minimum vocabulary size to trigger clustering.  Any document with less distinct words than the threshold will skip clustering
    */
  var minVocabSizeForClustering = 500

  /////////////maps and streams/////////////////

  //initial process for reading in values from file
  /**
    * Builds the main parallelized `Array` directly from file
    * @return `Array` of word and embedding values
    */
  def buildStreamFromFile: ParArray[(String, breeze.linalg.DenseVector[Double])] = {
    val iterator = scala.io.Source.fromInputStream(
                    unGZ(                                                     //automatically decompresses
                      getClass.getResource(this.w2vFilePath).getPath)
                    ).getLines

    //iterate through each line in [[vocabulary]]
    (for (wordRaw <- this.vocabulary.toList) yield {
      val word = wordRaw.toLowerCase                                                    //convert to lowercase
      val line = iterator.find(it => it.split(" ").head == word).getOrElse("zzz 0.0")   //find in iterator
      val splitLine = line.split(" ")                                                   //split string into elements
      val tail = splitLine.tail.map(_.toDouble)                                         //build w2v vector
      val vectorizedLine = splitLine.head -> breeze.linalg.DenseVector(tail)            //build map entry
      vectorizedLine
    }).toParArray
  }

  /**
    * Builds the main parallelized `Array` from [[w2vMasterMap]]
     * @return `Array` of word and embedding values
    */
  def buildStreamFromMap: ParArray[(String, breeze.linalg.DenseVector[Double])] = {
    //get the Option of the master map
    val map = this.w2vMasterMap.get
    //iterate through the vocabulary
    (for (wordRaw <- this.vocabulary.toList) yield {
      val word = wordRaw.toLowerCase    //convert to lowercase
      word -> map.getOrElse(word, breeze.linalg.DenseVector[Double](0d))
    }).toParArray
  }

  /**
    * The `Array` of word and embeddings only for words in [[vocabulary]], built either from file or [[w2vMasterMap]]
     */
  val w2vStream = if (this.w2vMasterMap.isEmpty) {
    this.buildStreamFromFile.
      filter(line => line._2 != breeze.linalg.DenseVector[Double](0d))    //filters out missing words from vocabulary
  } else {
    this.buildStreamFromMap.
      filter(line => line._2 != breeze.linalg.DenseVector[Double](0d))    //filters out missing words from vocabulary
  }

  //used to determine time of loading
  val stopInitialRead = System.currentTimeMillis().toDouble
  val initialReadLoadTime = (stopInitialRead - start) / 1000d

  /**
    * The dimension of the word embeddings
    */
  val dimension = if (this.w2vStream.isEmpty) 0 else this.w2vStream.head._2.length

  /**
    * Instance of `nak.cluster` for K-means clustering when size of [[vocabulary]] > [[minVocabSizeForClustering]]
    */
  val kmeans = new Kmeans[breeze.linalg.DenseVector[Double]](this.w2vStream.map(_._2).toVector)

  /**
    * The number of clusters to use for K-mean <br>
    *   Does not run if size of [[vocabulary]] > [[minVocabSizeForClustering]]
    */
  val k = if (this.vocabulary.length < this.minVocabSizeForClustering) {    //doesn't run clustering if the vocabulary size is less than global parameter
    0
  } else {
    this.vocabulary.length / 10
  }

  /**
    * Sequence of vectors representing the centroids as determined by K-means clustering
    */
  val centroids =   if (k != 0) {
                                  kmeans.run(k)._2
                                }
                    else  {
                            IndexedSeq(breeze.linalg.DenseVector.zeros[Double](this.dimension))
                          }

  //(cluster, index of word from initial vocabulary stream)
  /**
    * Sequence of tuples `(cluster index, index of word from initial vocabulary stream)`
    */
  val clusterMembership = kmeans.computeClusterMemberships(this.centroids)._2.zipWithIndex

  /**
    * Builds both an `Array` and a `Map` representation in one pass of the data <br>
    *   The first item in the tuple provides centroid information
    * @return (`Array` of `(cluster index, centroid) Array(word, embedding vector)`, (`Array` of `(cluster index, centroid) Map(word, embedding vector)`
    */
  def buildStreamPlusCentroid: (Array[((Int, DenseVector[Double]), Array[(String, DenseVector[Double])])], Array[((Int, DenseVector[Double]), ParMap[String, DenseVector[Double]])]) = {

    //buffer to allow building of the hash map in the same pass of the data
    val hashBuffer = collection.mutable.Buffer[((Int, DenseVector[Double]), ParMap[String, DenseVector[Double]])]()

    val stream = (for (centroidIndex <- this.centroids.indices) yield {
      //get the indices of w2v vectors in the cluster
      val indicesInCluster = this.clusterMembership.filter(tuple => tuple._1 == centroidIndex).map(_._2)
      //get the words in that cluster
      val wordsInCluster = (for (i <- indicesInCluster) yield {
          this.w2vStream(i)
      }).toArray

      //prepare the output tuple
      val toKeep = (
        (
          centroidIndex,
          this.centroids(centroidIndex)
        ),
          wordsInCluster
      )

      //add this to buffer
      hashBuffer += ((toKeep._1, toKeep._2.toMap.par))

      //return this for stream
      toKeep
    }).toArray
    (stream, hashBuffer.toArray)
  }

  /**
    * Output of [[buildStreamPlusCentroid]]
    */
  val (w2vStreamPlusCentroids, w2vHashMapPlusCentroids) = this.buildStreamPlusCentroid


  /**
    * Map of Word2vec embeddings when no clustering was implemented
    */
  val w2vFlatMapNoCentroids = this.w2vStream.toMap.par

  //used to determine time of loading
  val stop = System.currentTimeMillis().toDouble
  val overallLoadTime = (stop - start) / 1000d

  /////////////add and remove/////////////////

  //TODO build to handle clusters
  /*def addToHashMap(word: String): w2vHashMap.type = {
    val found = Source.fromFile(this.w2vFilePath).getLines.find(line =>
      line.split(" ").head == word
    )

    val w2vVector = DenseVector(found.get.split(" ").tail.toArray.map(_.toDouble))

    this.w2vHashMap += (word -> w2vVector)
  }*/

  //TODO build to handle clusters
  /*def removeFromHashMap(word: String): w2vHashMap.type = {
    this.w2vHashMap -= word
  }*/

  /////////////retrieval/////////////////

  /**
    * Retrieves the cluster index for a given word
    * @param word The word of interest
    * @return The index of the cluster in which the word's embedding is found
    */
  //get cluster in which the w2v vector resides
  def getClusterOfWord(word: String): Int = {
    //find the index of word in initial vocabulary list
    val wordIndex = this.vocabulary.indexOf(word)
    //find that index in the clusterMembership variable
    this.clusterMembership.find(_._2 == wordIndex).get._1
  }

  /**
    * Retrieves the cluster index for a given embedding vector
    * @param vector The embedding of interest
    * @param distance The distance metric to use: `manhattan`, `cosine similarity`, or `euclidean`; defaults to `euclidean`
    * @return The index of the cluster in which the embedding vector would be located
    */
  def getClusterOfVector(vector: breeze.linalg.DenseVector[Double], distance: String): Int = {
    if (distance == "euclidean") {
      val distanceTuple = for (c <- this.centroids) yield {
        this.centroids.indexOf(c) -> vectorDistance(c, vector, "euclidean")
      }

      distanceTuple.sortBy(_._2).head._1

    } else if (distance == "manhattan") {
      val distanceTuple = for (c <- this.centroids) yield {
        centroids.indexOf(c) -> vectorDistance(c, vector, "manhattan")
      }

      distanceTuple.sortBy(_._2).head._1

    } else if (distance == "cosine") {
      val distanceTuple = for (c <- this.centroids) yield {
        centroids.indexOf(c) -> cosSim(c, vector)
      }

      distanceTuple.sortBy(_._2).head._1

    } else {
      val distanceTuple = for (c <- this.centroids) yield {
        this.centroids.indexOf(c) -> vectorDistance(c, vector, "euclidean")
      }

      distanceTuple.sortBy(_._2).head._1
    }
  }


  /**
    * Retrieve the embedding vector for a given word
     * @param word The word of interest
    * @return The embedding vector for the word or a vector of 0s if word not found in original Word2Vec data
    */
  def getVector(word: String): breeze.linalg.DenseVector[Double] = {
    if (this.k == 1) {
      if (this.w2vStream.find(_._1 == word).isEmpty) {                         //if word wasn't in original word2vec data
        breeze.linalg.DenseVector.zeros[Double](this.dimension)
      } else {
        this.w2vHashMapPlusCentroids.head._2(word)
      }
    } else {
      if (this.w2vStream.find(_._1 == word).isEmpty) {                         //if word wasn't in original word2vec data
        breeze.linalg.DenseVector.zeros[Double](this.dimension)
      } else {
        val options = for (map <- w2vHashMapPlusCentroids.map(_._2)) yield {
          map.getOrElse(word, breeze.linalg.DenseVector[Double](0d))
        }
        options.find(each => each != breeze.linalg.DenseVector[Double](0d)).get
      }
    }
  }

  /**
    * Retrieves the closest word for a given embedding vector
    * @param w2vVector The vector of interest
    * @return The closest word and its cosine similarity to the input vector
    */
  //get word (or closest word) for given w2v vector
  def getWord(w2vVector: breeze.linalg.DenseVector[Double]): (String, Double) = {
    //if only one cluster
    if (this.k == 1) {
      val word = this.w2vStream.find(w2v =>
        w2v._2 == w2vVector)

      if (word.isEmpty) {                                                                             //if there is no match
        val closestWord = findClosestWord(w2vVector, 1).head._1                                         //find closest word

        closestWord -> cosSim(w2vStream.find(w2v => w2v._1 == closestWord).get._2, w2vVector)         //return (word -> similarity score)
      } else {
        word.get._1 -> 1.0
      }
    //if clustering occurred
    } else {
      val clusterIndex = (for (centroid <- this.w2vStreamPlusCentroids) yield {
        centroid._1._1 -> cosSim(centroid._1._2, w2vVector)
      }).sortBy(_._2).reverse.take(1).head._1

      val word = this.w2vStreamPlusCentroids(clusterIndex)._2.find(w2v =>
        w2v._2 == w2vVector)

      if (word.isEmpty) {                                                                             //if there is no match
      val closestWord = findClosestWord(w2vVector, 1).head._1                                           //find closest word

        closestWord -> cosSim(this.w2vStream.find(w2v => w2v._1 == closestWord).get._2, w2vVector)    //return (word -> similarity score)
      } else {
        word.get._1 -> 1.0
      }
    }
  }

  /////////////word2vec functions/////////////////

  //finds closest words to given vector
    //returns (word -> similarity)
    //take ==> parameter of how many entries to return
  /**
    * Retrieves closest `n` words to a given vector <br>
    *   For efficiency, only looks for matching words in closest cluster according to `euclidean` distance and the closest cluster according to `cosine similarity` (in case those clusters differ)
    * @param w2vVector The vector of interest
    * @param take The number of closest words (`n`) to return
    * @return `Vector` of words and their cosine similarity to input vector
    */
  def findClosestWord(w2vVector: breeze.linalg.DenseVector[Double], take: Int): scala.collection.immutable.Vector[(String, Double)] = {

    //if no clustering occurred
    if (k == 1) {
      (for (word <- this.w2vStream) yield {                        //iterate through all vectors
        word._1 -> cosSim(word._2, w2vVector)              //calculate cosine similarity to given vector and return (word -> similarity)
      }).toVector.sortBy(_._2).reverse.take(take)         //sort by highest similarity and return parameterized number of entries

    //if clustering did occur
    } else {

      /*//calculating cosSim for all words
      val allCosSimScores = for (word <- this.w2vFlatMapNoCentroids.keySet) yield {
        word -> this.w2vCosSim(this.w2vFlatMapNoCentroids(word), w2vVector)
      }

      allCosSimScores.toVector.distinct.sortBy(_._2).reverse.take(take)*/

      //TODO fix this part - add closest X centroids
        //as example => collaborator and collaboration not in same clusters yet close (cosSim = .45)

      //take words from same cluster using euclidean distance
      val sameCluster = this.getClusterOfVector(w2vVector, "euclidean")

      val sameClusterMap = this.w2vStreamPlusCentroids.find(_._1._1 == sameCluster).get._2      //find the cluster by its index

      val sameClusterWords = (for (word <- sameClusterMap) yield {                        //iterate through all vectors in closest cluster map
        word._1 -> cosSim(word._2, w2vVector)                                                //calculate cosine similarity to given vector and return (word -> similarity)
      }).toVector.sortBy(_._2).reverse.take(take * 2)                                         //sort by highest similarity and return parameterized number of entries times two (to later be melded with words from same cluster)

      //take words from mostSimilar (using cosSim) cluster
      val closestCluster = (for (centroid <- this.w2vStreamPlusCentroids) yield {      //iterate through all centroids
        centroid._1._1 -> cosSim(centroid._1._2, w2vVector)                             //calculate cosine similarity to given vector with centroid
      }).toVector.sortBy(_._2).reverse.head                                               //take highest cluster

      val closestClusterMap = this.w2vStreamPlusCentroids.find(_._1._1 == closestCluster._1).get._2      //find the cluster by its index

      val closestClusterWords = (for (word <- closestClusterMap) yield {                        //iterate through all vectors in closest cluster map
        word._1 -> cosSim(word._2, w2vVector)                                                     //calculate cosine similarity to given vector and return (word -> similarity)
        }).toVector.sortBy(_._2).reverse.take(take * 2)                                             //sort by highest similarity and return parameterized number of entries times two (to later be melded with words from same cluster)

      //get closest overall
      val combined = closestClusterWords ++ sameClusterWords                                    //merge the two lists

      combined.distinct.sortBy(_._2).reverse.take(take)                                         //sort the merged list and return parameterized number of items

    }
  }


  /**
    * Retrieves all words with a cosine similarity above a certain threshold
    * @param w2vVector The vector of interest
    * @param cosSimThreshold The minimum cosine similarity score of words to return
    * @return `Vector` of words and their cosine similarity to the input vector
    */
  def findClosestWord(w2vVector: breeze.linalg.DenseVector[Double], cosSimThreshold: Double): scala.collection.immutable.Vector[(String, Double)] = {

    //if no clustering occurred
    if (k == 1) {
      val allCosSim = (for (word <- w2vStream) yield {      //iterate through all vectors
        word._1 -> cosSim(word._2, w2vVector)                 //calculate cosine similarity to given vector and return (word -> similarity)
      }).toVector.sortBy(_._2).reverse                          //sort by highest similarity and

      allCosSim.takeWhile(_._2 >= cosSimThreshold)               //return parameterized number of entries


    //if clustering did occur
    } else {

      /*//calculating cosSim for all words
      val allCosSimScores = for (word <- this.w2vFlatMapNoCentroids.keySet) yield {
                            word -> this.w2vCosSim(this.w2vFlatMapNoCentroids(word), w2vVector)
                            }

      allCosSimScores.toVector.distinct.sortBy(_._2).reverse.takeWhile(_._2 > cosSimThreshold)*/

      //TODO fix this part - add closest X centroids
        //as example => collaborator and collaboration not in same clusters yet close (cosSim = .45)
      //take words from same cluster using euclidean distance
      val sameCluster = this.getClusterOfVector(w2vVector, "euclidean")

      val sameClusterMap = this.w2vStreamPlusCentroids.find(_._1._1 == sameCluster).get._2      //find the cluster by its index

      val sameClusterWords = (for (word <- sameClusterMap) yield {                        //iterate through all vectors in closest cluster map
        word._1 -> cosSim(word._2, w2vVector)                                               //calculate cosine similarity to given vector and return (word -> similarity)
      }).toVector.sortBy(_._2).reverse                                                        //sort by highest similarity

      //take words from mostSimilar (using cosSim) cluster
      val closestCluster = (for (centroid <- this.w2vStreamPlusCentroids) yield {         //iterate through all centroids
        centroid._1._1 -> cosSim(centroid._1._2, w2vVector)                                 //calculate cosine similarity to given vector with centroid
      }).toVector.sortBy(_._2).reverse.head                                                   //take highest cluster

      val closestClusterMap = this.w2vStreamPlusCentroids.find(_._1._1 == closestCluster._1).get._2      //find the cluster by its index

      val closestClusterWords = (for (word <- closestClusterMap) yield {                        //iterate through all vectors in closest cluster map
        word._1 -> cosSim(word._2, w2vVector)                                                     //calculate cosine similarity to given vector and return (word -> similarity)getVector
      }).toVector.sortBy(_._2).reverse                                                              //sort by highest similarity

      //get closest overall
      val combined = closestClusterWords ++ sameClusterWords                                      //merge the two lists

      combined.distinct.sortBy(_._2).reverse.takeWhile(_._2 > cosSimThreshold)                   //sort the merged list and return numbers above parameterized cosSim

    }
  }



}

/**
  * Contains methods used to build the original [[Word2Vec.w2vMasterMap]] that can be used as input to [[Word2Vec]]
  */
object Word2Vec {

  /**
    * Loads original Word2vec file and converts to a `Map` of Strings.  The conversion to `breeze.linalg.DenseVector` occurs in [[Word2Vec]] class
    * @param w2vPath location of original Word2Vec file
    * @param take Number of entries to take from original Word2Vec file <br>
    * The top 500k words should be sufficient for most needs
    * @return Map to be used as [[Word2Vec.w2vMasterMap]]
    * @deprecated [[makeMutableMapDense]] is used instead
    */
  def makeMutableMap(w2vPath: String, take: Int): collection.mutable.Map[String, Array[String]] = {
    val start = System.currentTimeMillis().toDouble
    val iterator = scala.io.Source.fromInputStream(unGZ(getClass.getResource(w2vPath).getPath)).getLines()
    val map = collection.mutable.Map.empty[String, Array[String]]
    var c = 0
    breakable {
      for (l <- iterator) {
        c += 1
        if (c <= take) {
          val line = l.split(" ")
          val word = line.head
          if (word.matches("""^\D.*""")) {
            map(word) = line.tail
          }
        } else {
          break
        }
      }
    }
    val stop = System.currentTimeMillis().toDouble
    val elapsed = (stop - start) / 1000d
    println(elapsed + " seconds")
    map
  }


  /**
    * Loads original Word2vec file and converts to a `Map` of `breeze.linalg.DenseVector`
    * @param w2vPath location of original Word2Vec file
    * @param take Number of entries to take from original Word2Vec file <br>
    * The top 500k words should be sufficient for most needs
    * @return Map to be used as [[Word2Vec.w2vMasterMap]]
    */
  def makeMutableMapDense(w2vPath: String, take: Int): collection.mutable.Map[String, breeze.linalg.DenseVector[Double]] = {
    val start = System.currentTimeMillis().toDouble
//    val iterator = Source.fromInputStream(unGZ(getClass.getResource(w2vPath).getPath)).getLines()
    val iterator = scala.io.Source.fromInputStream(unGZ(getClass.getResourceAsStream(w2vPath))).getLines()
    val map = collection.mutable.Map.empty[String, breeze.linalg.DenseVector[Double]]
    var c = 0
    breakable {
      for (l <- iterator) {
        c += 1
        if (c <= take) {
          val line = l.split(" ")
          val word = line.head
          if (word.matches("""^\D.*""")) {
            if (c % 1000 == 0) print(c.toString + "\r\r\r\r\r\r")
            val tail = line.tail.map(_.toDouble)
            val vectorizedTail = breeze.linalg.DenseVector(tail)
            map(line.head) = vectorizedTail
          }
        } else {
          break
        }
      }
    }
    val stop = System.currentTimeMillis().toDouble
    val elapsed = (stop - start) / 1000d
    println("loading w2v: " + elapsed + " seconds")
    map
  }

}

