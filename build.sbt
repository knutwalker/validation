lazy val core = project settings (name := "validation")

lazy val scalaz = project dependsOn core settings (
  dontRelease,
  name := "validation-scalaz",
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.2")

lazy val tests = project dependsOn (core, scalaz) settings (
  dontRelease,
  resolvers += "Scalaz" at "https://dl.bintray.com/scalaz/releases",
  scalacOptions in Test ~= (_.filterNot(_ == "-Ywarn-dead-code")),
  libraryDependencies ++= List(
    "org.specs2"     %% "specs2-core"          % "3.6"    % "test",
    "org.specs2"     %% "specs2-html"          % "3.6"    % "test",
    "org.specs2"     %% "specs2-matcher-extra" % "3.6"    % "test",
    "org.specs2"     %% "specs2-scalacheck"    % "3.6"    % "test",
    "org.scalacheck" %% "scalacheck"           % "1.12.2" % "test",
    "org.typelevel"  %% "scalaz-specs2"        % "0.4.0"  % "test"
      exclude("org.specs2", s"specs2-core${scalaBinaryVersion.value}")
      exclude("org.specs2", s"specs2-scalacheck${scalaBinaryVersion.value}")))

lazy val docs = project dependsOn tests

lazy val parent = project in file(".") dependsOn (core, scalaz) aggregate (core, scalaz, tests, docs) settings dontRelease

addCommandAlias("travis", ";clean;coverage;test;coverageReport;coverageAggregate")
addCommandAlias("ghDocs", ";tests/testOnly -- html html.nostats;makeSite;ghpagesPushSite")
