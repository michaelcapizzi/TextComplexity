logLevel := Level.Warn

// used for building project website
resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"
addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.4")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.0")
