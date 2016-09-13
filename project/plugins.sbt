

resolvers ++= Seq(
  "Artima Maven Repository" at "http://repo.artima.com/releases",
  "Spark Packages Repo Bintray" at "https://dl.bintray.com/spark-packages/maven/",
  "Sonatype releases" at "https://oss.sonatype.org/content/repositories/releases"
)


addSbtPlugin("org.ensime" % "sbt-ensime" % "1.9.0")
