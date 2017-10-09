lazy val core = project settings (
  name := "validation",
  scalacOptions in Compile ~= (_.filterNot(Set("-Yclosure-elim", "-Yconst-opt", "-Ydead-code")))
)

lazy val scalaz = project dependsOn core settings (
  dontRelease,
  name := "validation-scalaz",
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.15")

lazy val tests = project dependsOn (core, scalaz) settings (
  dontRelease,
  resolvers += "Scalaz" at "https://dl.bintray.com/scalaz/releases",
  scalacOptions in Test ~= (_.filterNot(Set("-Yclosure-elim", "-Yconst-opt", "-Ydead-code", "-Ywarn-dead-code"))),
  libraryDependencies ++= List(
    "org.specs2"     %% "specs2-core"          % "3.9.5"    % "test",
    "org.specs2"     %% "specs2-html"          % "3.9.5"    % "test",
    "org.specs2"     %% "specs2-matcher-extra" % "3.9.5"    % "test",
    "org.specs2"     %% "specs2-scalacheck"    % "3.9.5"    % "test",
    "org.scalacheck" %% "scalacheck"           % "1.13.5"   % "test",
    "org.typelevel"  %% "scalaz-specs2"        % "0.5.0"    % "test"
      exclude("org.specs2", s"specs2-core${scalaBinaryVersion.value}")
      exclude("org.specs2", s"specs2-scalacheck${scalaBinaryVersion.value}")))

lazy val docs = project dependsOn tests settings (
  dontRelease,
  scalacOptions in Compile ~= (_.filterNot(Set("-Yclosure-elim", "-Yconst-opt", "-Ydead-code")))
)

lazy val parent = project in file(".") dependsOn (core, scalaz) aggregate (core, scalaz, tests, docs) settings dontRelease

addCommandAlias("travis", ";clean;coverage;test;coverageReport;coverageAggregate")
addCommandAlias("ghDocs", ";tests/testOnly -- html html.nostats;makeSite;ghpagesPushSite")
