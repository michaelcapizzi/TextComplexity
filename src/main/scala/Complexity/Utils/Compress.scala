package Utils

import java.io._
import java.util.zip.{ZipEntry, ZipOutputStream, GZIPInputStream}

/**
 * Created by mcapizzi on 9/9/15.
 */
object Compress {

  //uncompresses file into Stream
  def unGZ(fileName: String): InputStream = {
    new GZIPInputStream(new BufferedInputStream(new FileInputStream(fileName)))
  }

  //uncompresses InputStream
  def unGZ(stream: InputStream): InputStream = {
    new GZIPInputStream(stream)
  }

  //method specifically for opening EOS model
  def unGZEOSModel(fileName: String): InputStreamReader = {
//    new InputStreamReader(new GZIPInputStream(new BufferedInputStream(new FileInputStream(fileName))))
//    new InputStreamReader(new GZIPInputStream(new BufferedInputStream(getClass.getResourceAsStream(fileName))))
    new InputStreamReader(new GZIPInputStream(getClass.getResourceAsStream(fileName)))
  }

  //compress files into .gz
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
