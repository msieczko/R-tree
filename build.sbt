name := "jps.RTree"

version := "0.1"

scalaVersion := "2.11.12"

//resolvers += "bintray/meetup" at "http://dl.bintray.com/meetup/maven"
//resolvers += DefaultMavenRepository

resolvers += Resolver.bintrayRepo("meetup", "maven")
libraryDependencies += "com.meetup" %% "archery" % "0.4.0"

