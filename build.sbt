name := "jps.RTree"

version := "0.1"

scalaVersion := "2.11.12"

resolvers += Resolver.bintrayRepo("meetup", "maven")
libraryDependencies += "com.meetup" %% "archery" % "0.4.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
libraryDependencies += "com.github.nikita-volkov" % "sext" % "0.2.4"

