
import sbt._
import Keys._

object BuildSettings {

  val sharedSettings = Seq(
    organization := "net.clhodapp",
    version := "0.0.1",
    scalaVersion := "2.10.1",
    scalacOptions ++= Seq("-feature", "-deprecation")
  )

  val reflectiveSettings = Seq(
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
  )

  val subDirectorySettings = Seq(
    sourceDirectory <<= (baseDirectory)(identity)
  )

}

object TuplicityBuild extends Build {
  import Defaults._
  import BuildSettings._

  lazy val mainProject: Project = Project(
    id = "tuplicity",
    base = file("."),
    settings = defaultSettings ++ sharedSettings ++  Seq(
        libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
      )
  ) aggregate(format) dependsOn(format)

  lazy val format: Project = Project(
    id = "tuplicity_format",
    base = file("format"),
    settings = 
      defaultSettings ++ sharedSettings ++ reflectiveSettings ++ subDirectorySettings 
   )

}

