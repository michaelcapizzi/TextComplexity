package Complexity.Features

import breeze.linalg._
import Complexity.SupportMethods.Similarity._
import nak.cluster._
import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.mutable.ParArray
import scala.io.Source
import Utils.Compress._
import org.json4s._
import org.json4s.native.Serialization._
import org.json4s.native.Serialization

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
  def buildStreamFromFile: ParArray[(String, breeze.linalg.DenseVector[Double])] = {
    val iterator = Source.fromInputStream(unGZ(getClass.getResource(this.w2vFilePath).getPath)).getLines
    (for (wordRaw <- this.vocabulary.toList) yield {
      val word = wordRaw.toLowerCase                                                    //convert to lowercase
      val line = iterator.find(it => it.split(" ").head == word).getOrElse("zzz 0.0")   //find in iterator
      val splitLine = line.split(" ")                                                   //split string into elements
      val tail = splitLine.tail.map(_.toDouble)                                         //build w2v vector
      val vectorizedLine = splitLine.head -> breeze.linalg.DenseVector(tail)            //build map entry
      vectorizedLine
    }).toParArray
  }

    //get methods later have a means for building a default value
  def buildStreamFromMap: ParArray[(String, breeze.linalg.DenseVector[Double])] = {
    //get the Option of the master map
    val map = this.w2vMasterMap.get
    //iterate through the vocabulary
    (for (wordRaw <- this.vocabulary.toList) yield {
      val word = wordRaw.toLowerCase    //convert to lowercase
      word -> map.getOrElse(word, breeze.linalg.DenseVector[Double](0d))
    }).toParArray
  }

  //build the stream
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

  //dimension of word2vec vectors
  val dimension = if (this.w2vStream.isEmpty) 0 else this.w2vStream.head._2.length

  //clustering - for any case where the vocabulary size is greater than 100 distinct words
  val kmeans = new Kmeans[breeze.linalg.DenseVector[Double]](this.w2vStream.map(_._2).toVector)

  val k = if (this.vocabulary.length < this.minVocabSizeForClustering) {    //doesn't run clustering if the vocabulary size is less than global parameter
    0
  } else {
    this.vocabulary.length / 10
  }

  val centroids =   if (k != 0) {
                                  kmeans.run(k)._2
                                }
                    else  {
                            IndexedSeq(breeze.linalg.DenseVector.zeros[Double](this.dimension))
                          }

  //(cluster, index of word from initial vocabulary stream)
  val clusterMembership = kmeans.computeClusterMemberships(this.centroids)._2.zipWithIndex

  //stream and hashmap with a leading tuple that includes centroid information
    //(cluster index, centroid vector), Map(word -> vector))
  def buildStreamPlusCentroid: (Array[((Int, DenseVector[Double]), Array[(String, DenseVector[Double])])], Array[((Int, DenseVector[Double]), ParMap[String, DenseVector[Double]])]) = {

    //to build the hashMap at the same time
    val hashBuffer = collection.mutable.Buffer[((Int, DenseVector[Double]), ParMap[String, DenseVector[Double]])]()

    val stream = (for (centroidIndex <- this.centroids.indices) yield {
      val indicesInCluster = this.clusterMembership.filter(tuple => tuple._1 == centroidIndex).map(_._2)           //get the indices of w2v vectors in the cluster
      val wordsInCluster = (for (i <- indicesInCluster) yield {
          this.w2vStream(i)
      }).toArray

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

  //handled by buildStreamPlusCentroid above
  /*def buildHashMapPlusCentroid: Array[((Int, DenseVector[Double]), ParMap[String, DenseVector[Double]])] = {
    for (cluster <- this.w2vStreamPlusCentroids) yield {
      (
        cluster._1,
        cluster._2.toMap.par
      )
    }
  }*/

  val (w2vStreamPlusCentroids, w2vHashMapPlusCentroids) = this.buildStreamPlusCentroid

//  val w2vHashMapPlusCentroids = this.buildHashMapPlusCentroid

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

  //get cluster in which the w2v vector resides
  def getClusterOfWord(word: String): Int = {
    val wordIndex = this.vocabulary.indexOf(word)                   //find the index of word in initial vocabulary list
    this.clusterMembership.find(_._2 == wordIndex).get._1           //find that index in the clusterMembership variable
  }

  //uses distance to find closest cluster
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


  //get w2v vector for given word
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

  //get word (or closest word) for given w2v vector
  def getWord(w2vVector: breeze.linalg.DenseVector[Double]): (String, Double) = {
    if (this.k == 1) {
      val word = this.w2vStream.find(w2v =>
        w2v._2 == w2vVector)

      if (word.isEmpty) {                                                                             //if there is no match
        val closestWord = findClosestWord(w2vVector, 1).head._1                        //find closest word

        closestWord -> cosSim(w2vStream.find(w2v => w2v._1 == closestWord).get._2, w2vVector)      //return (word -> similarity score)
      } else {
        word.get._1 -> 1.0
      }
    } else {
      val clusterIndex = (for (centroid <- this.w2vStreamPlusCentroids) yield {
        centroid._1._1 -> cosSim(centroid._1._2, w2vVector)
      }).sortBy(_._2).reverse.take(1).head._1

      val word = this.w2vStreamPlusCentroids(clusterIndex)._2.find(w2v =>
        w2v._2 == w2vVector)

      if (word.isEmpty) {                                                                             //if there is no match
      val closestWord = findClosestWord(w2vVector, 1).head._1                        //find closest word

        closestWord -> cosSim(this.w2vStream.find(w2v => w2v._1 == closestWord).get._2, w2vVector)      //return (word -> similarity score)
      } else {
        word.get._1 -> 1.0
      }
    }
  }

  /////////////word2vec functions/////////////////

  //finds closest words to given vector
    //returns (word -> similarity)
    //take ==> parameter of how many entries to return
  def findClosestWord(w2vVector: breeze.linalg.DenseVector[Double], take: Int): scala.collection.immutable.Vector[(String, Double)] = {

    if (k == 1) {
      (for (word <- this.w2vStream) yield {                        //iterate through all vectors
        word._1 -> cosSim(word._2, w2vVector)              //calculate cosine similarity to given vector and return (word -> similarity)
      }).toVector.sortBy(_._2).reverse.take(take)         //sort by highest similarity and return parameterized number of entries

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
        centroid._1._1 -> cosSim(centroid._1._2, w2vVector)                    //calculate cosine similarity to given vector with centroid
      }).toVector.sortBy(_._2).reverse.head                                       //take highest cluster

      val closestClusterMap = this.w2vStreamPlusCentroids.find(_._1._1 == closestCluster._1).get._2      //find the cluster by its index

      val closestClusterWords = (for (word <- closestClusterMap) yield {                        //iterate through all vectors in closest cluster map
        word._1 -> cosSim(word._2, w2vVector)                                                //calculate cosine similarity to given vector and return (word -> similarity)
        }).toVector.sortBy(_._2).reverse.take(take * 2)                                         //sort by highest similarity and return parameterized number of entries times two (to later be melded with words from same cluster)

      //get closest overall
      val combined = closestClusterWords ++ sameClusterWords                                    //merge the two lists

      combined.distinct.sortBy(_._2).reverse.take(take)                                                  //sort the merged list and return parameterized number of items

    }
  }


  //finds closest words above a certain cosSim threshold
  //returns (word -> similarity)
  //take ==> parameter of how many entries to return
  def findClosestWord(w2vVector: breeze.linalg.DenseVector[Double], cosSimThreshold: Double): scala.collection.immutable.Vector[(String, Double)] = {

    if (k == 1) {
      val allCosSim = (for (word <- w2vStream) yield {                        //iterate through all vectors
        word._1 -> cosSim(word._2, w2vVector)              //calculate cosine similarity to given vector and return (word -> similarity)
      }).toVector.sortBy(_._2).reverse                        //sort by highest similarity and

      allCosSim.takeWhile(_._2 >= cosSimThreshold)               //return parameterized number of entries


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
        word._1 -> cosSim(word._2, w2vVector)                                                //calculate cosine similarity to given vector and return (word -> similarity)
      }).toVector.sortBy(_._2).reverse                                                    //sort by highest similarity

      //take words from mostSimilar (using cosSim) cluster
      val closestCluster = (for (centroid <- this.w2vStreamPlusCentroids) yield {      //iterate through all centroids
        centroid._1._1 -> cosSim(centroid._1._2, w2vVector)                    //calculate cosine similarity to given vector with centroid
      }).toVector.sortBy(_._2).reverse.head                                       //take highest cluster

      val closestClusterMap = this.w2vStreamPlusCentroids.find(_._1._1 == closestCluster._1).get._2      //find the cluster by its index

      val closestClusterWords = (for (word <- closestClusterMap) yield {                        //iterate through all vectors in closest cluster map
        word._1 -> cosSim(word._2, w2vVector)                                                //calculate cosine similarity to given vector and return (word -> similarity)getVector
      }).toVector.sortBy(_._2).reverse                                                          //sort by highest similarity

      //get closest overall
      val combined = closestClusterWords ++ sameClusterWords                                    //merge the two lists

      combined.distinct.sortBy(_._2).reverse.takeWhile(_._2 > cosSimThreshold)                   //sort the merged list and return numbers above parameterized cosSim

    }
  }



}

object Word2Vec {

  //loads word2vec file and generates master map
    //note: master map values are just Array[String] --> conversion to breeze.DenseVector happens in Word2Vec class
  def makeMutableMap(w2vPath: String, take: Int): collection.mutable.Map[String, Array[String]] = {
    val start = System.currentTimeMillis().toDouble
    val iterator = Source.fromInputStream(unGZ(getClass.getResource(w2vPath).getPath)).getLines()
    val map = collection.mutable.Map.empty[String, Array[String]]
    var c = 0
    breakable {
      for (l <- iterator) {
        c += 1
        if (c <= take) {
          val line = l.split(" ")
          val word = line.head
          if (word.matches("""^\D.*""")) {
//            if (c % 1000 == 0) println(c.toString + ":" + ((System.currentTimeMillis().toDouble - start) / 1000d).toString)
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


  //loads map with DenseVectors as values
    //take = number of items to take from list (ordered by frequency)
  def makeMutableMapDense(w2vPath: String, take: Int): collection.mutable.Map[String, breeze.linalg.DenseVector[Double]] = {
    val start = System.currentTimeMillis().toDouble
//    val iterator = Source.fromInputStream(unGZ(getClass.getResource(w2vPath).getPath)).getLines()
    val iterator = Source.fromInputStream(unGZ(getClass.getResourceAsStream(w2vPath))).getLines()
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


//////////////////////////////////////////////////////

  //makes a json version of word2vec list
  def makeJSONMap(w2vFilePath: String): String = {
    implicit val formats = Serialization.formats(NoTypeHints)

    val startsWithNumber = """^\d""".r
    val w2vLookUpMap = collection.mutable.Map[String, collection.immutable.Vector[Double]]()

//    for (line <- Source.fromInputStream(unGZ(getClass.getResource(w2vFilePath).getPath)).getLines) {
    for (line <- Source.fromInputStream(unGZ(getClass.getResourceAsStream(w2vFilePath))).getLines) {
      if (startsWithNumber.findFirstIn(line).isEmpty) {
        val split = line.split(" ")
        val word = split.head
        val values = split.tail.map(_.toDouble).toVector
        println("adding " + word)
        w2vLookUpMap += (word -> values)
//        w2vLookUpMap put (word, values)

      }
    }
    println("writing to json")
    write(w2vLookUpMap)

    //loading
    //parse(z).extract[Map[String, List[Double]]]
  }

  //generates alphatized list of word2vec
  def alphabetizeW2V(w2vFilePath: String): Array[String] = {
    println("loading lines")
//    val lines = Source.fromInputStream(unGZ(getClass.getResource(w2vFilePath).getPath)).getLines.toArray
    val lines = Source.fromInputStream(unGZ(getClass.getResourceAsStream(w2vFilePath))).getLines.toArray
    println("sorting")
    lines.sorted
  }

}

