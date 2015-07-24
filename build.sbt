
scalacOptions ++= Seq("-feature")

//val root = Project("fpinscala-ensime-test", file(".")).setPlugins(

// ignored;
// add dependencies to 'opts' in Build.scala
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.11.7"
)
