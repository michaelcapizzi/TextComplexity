package Complexity.Utils

import java.io._
import java.util.zip.{ZipEntry, ZipOutputStream, GZIPInputStream}

/**
 * Supporting methods for handling compressed files
 */
object Compress {

  /**
    * Uncompresses file into `InputStream`
    * @param fileName File to be uncompressed
    */
  def unGZ(fileName: String): InputStream = {
    new GZIPInputStream(new BufferedInputStream(new FileInputStream(fileName)))
  }

  /**
    * Uncompresses an `InputStream`
    * @param stream `InputStream` to be uncompressed
    */
  def unGZ(stream: InputStream): InputStream = {
    new GZIPInputStream(stream)
  }

  /**
    * Method for specifically opening a compressed, trained classifier for use in [[Complexity.MachineLearning.Model]]
    * @param fileName File housing saved model
    */
  def unGZmlModel(fileName: String): InputStreamReader = {
//    new InputStreamReader(new GZIPInputStream(new BufferedInputStream(new FileInputStream(fileName))))
//    new InputStreamReader(new GZIPInputStream(new BufferedInputStream(getClass.getResourceAsStream(fileName))))
    new InputStreamReader(new GZIPInputStream(getClass.getResourceAsStream(fileName)))
  }

  /**
    * Compresses a given file into `.gz`
    * @param zipOutput Location of output `.gz` file
    * @param listOfFiles List of files to be compressed
    */
  def GZ(zipOutput: String, listOfFiles: Vector[File]): Unit = {
    val zip = new ZipOutputStream(new FileOutputStream(zipOutput))

    listOfFiles.foreach { f =>
      zip.putNextEntry(new ZipEntry(f.getName))
      val in = new BufferedInputStream(new FileInputStream(f.getAbsolutePath))
      var b = in.read
      while (b > -1) {
        zip.write(b)
        b = in.read
      }
      in.close
      zip.closeEntry
    }
    zip.close()
  }





}
