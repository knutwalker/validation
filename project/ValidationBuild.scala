import sbt._
import sbt.Keys._
import de.knutwalker.sbt._
import de.knutwalker.sbt.KSbtKeys._

object ValidationBuild extends AutoPlugin {
  override def trigger = allRequirements
  override def requires = KSbtPlugin

  override lazy val projectSettings = List(
                organization := "de.knutwalker",
                   startYear := Some(2015),
                  maintainer := "Paul Horn",
               githubProject := Github("knutwalker", "validation"),
                 description := "stand-alone validation type",
                scalaVersion := "2.11.6",
          crossScalaVersions := scalaVersion.value :: "2.10.5" :: Nil,
  initialCommands in console := """import validation._"""
  )
}
