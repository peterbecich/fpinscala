import sbt._
import Keys._

import org.ensime.EnsimePlugin

object FPInScalaBuild extends Build {
  val algebirdVersion = "0.12.1"

  val kindProjector = compilerPlugin("org.spire-math" % "kind-projector" % "0.8.0" cross CrossVersion.binary)
  val si2712 = compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)

  val opts = Project.defaultSettings ++ Seq(
    scalaVersion := "2.11.8",
    scalacOptions ++= Seq("-feature","-deprecation"),
    resolvers ++= Seq(
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % "2.11.8",
      "com.chuusai" %% "shapeless" % "2.2.5",
      "com.twitter" %% "algebird-core" % algebirdVersion,
      "com.twitter" %% "algebird-test" % algebirdVersion,
      "com.twitter" %% "algebird-util" % algebirdVersion,
      "com.twitter" %% "algebird-bijection" % algebirdVersion,
      kindProjector,
      si2712


    )
  )

  lazy val root =
    Project(id = "root",
            base = file("."),
            settings = opts ++ Seq(
              onLoadMessage ~= (_ + nio2check())
            ) ++ EnsimePlugin.projectSettings).aggregate(chapterCode, exercises, answers)
  lazy val chapterCode =
    Project(id = "chapter-code",
            base = file("chaptercode"),
            settings = opts)
  lazy val exercises =
    Project(id = "exercises",
            base = file("exercises"),
            settings = opts)
  lazy val answers =
    Project(id = "answers",
            base = file("answers"),
            settings = opts)

  def nio2check(): String = {
    val cls = "java.nio.channels.AsynchronousFileChannel"
    try {Class.forName(cls); ""}
    catch {case _: ClassNotFoundException =>
      ("\nWARNING: JSR-203 \"NIO.2\" (" + cls + ") not found.\n" +
       "You are probably running Java < 1.7; answers will not compile.\n" +
       "You seem to be running " + System.getProperty("java.version") + ".\n" +
       "Try `project exercises' before compile, or upgrading your JDK.")
    }
  }
}
