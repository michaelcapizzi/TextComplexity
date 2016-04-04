import com.typesafe.sbt.GitPlugin.autoImport._
import com.typesafe.sbt.SbtGhPages.ghpages
import com.typesafe.sbt.SbtSite.site
import sbtassembly.AssemblyPlugin.autoImport._
import sbtassembly.MergeStrategy

name := "TextComplexity"

version := "1.0"

scalaVersion := "2.11.7"

ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }      //this overrides competing scala versions?  ???

assemblyJarName in assembly := "TextComplexityDemo.jar"

test in assembly := {}

mainClass in assembly := Some("Complexity.Demo")

assemblyExcludedJars in assembly := {
  val cp = (fullClasspath in assembly).value
  cp filter {_.data.getName == "java-cup-0.11a.jar"}
}

libraryDependencies ++= Seq(
  "edu.arizona.sista" % "processors_2.11" % "5.5",                          //required exclude java-cup-0.11a.jar
  "edu.arizona.sista" % "processors_2.11" % "5.5" classifier "models",
//  "org.clulab" %% "processors" % "5.8.1",
//  "org.clulab" %% "processors" % "5.8.1" classifier "models",
  "org.apache.commons" % "commons-math3" % "3.3",
  "org.apache.commons" % "commons-compress" % "1.9",
//  "org.apache.commons" % "commons-io" % "1.3.2",
  "com.quantifind" % "wisp_2.11" % "0.0.4",
  "org.scalanlp" % "breeze-natives_2.11" % "0.11.2",
  //  "org.scalanlp" % "breeze_2.11" % "0.11.2",
  "org.scalanlp" %% "breeze-viz" % "0.11.2",
  "org.scalanlp" % "nak_2.11" % "1.3",
//  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.2",                        //had a bug with MWE in Grapes of Wrath
//  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.2" classifier "models",    //had a bug with MWE in Grapes of Wrath
  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.1",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.1" classifier "models",
  "org.scalactic" %% "scalactic" % "2.2.6",                               //for unit tests
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)

// settings for building project website
// https://github.com/sbt/sbt-site
site.settings

// include documentation
site.includeScaladoc()

// https://github.com/sbt/sbt-ghpages
ghpages.settings

git.remoteRepo := "git@github.com:michaelcapizzi/TextComplexity.git"