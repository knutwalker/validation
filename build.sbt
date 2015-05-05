lazy val core = project settings (name := "validation")

lazy val shapeless = project dependsOn core settings (
  dontRelease,
  name := "validation-shapeless",
  libraryDependencies += "com.chuusai" %% "shapeless" % "2.2.0-RC5")

lazy val scalaz = project dependsOn core settings (
  dontRelease,
  name := "validation-scalaz",
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.2")

lazy val tests = project dependsOn (core, shapeless, scalaz) settings (
  dontRelease,
  resolvers ++= List(
    Resolver.sonatypeRepo("snapshots"),
    "Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases"
  ),
  libraryDependencies ++= List(
    "org.specs2"                 %% "specs2-core"               % "3.6"              % "test",
    "org.specs2"                 %% "specs2-scalacheck"         % "3.6"              % "test",
    "org.specs2"                 %% "specs2-matcher-extra"      % "3.6"              % "test",
    "org.scalacheck"             %% "scalacheck"                % "1.12.2"           % "test",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.12" % "0.2.0-SNAPSHOT"   % "test",
    "org.typelevel"              %% "scalaz-specs2"             % "0.4.0"            % "test"
      exclude("org.specs2", s"specs2-core${scalaBinaryVersion.value}")
      exclude("org.specs2", s"specs2-scalacheck${scalaBinaryVersion.value}")))

lazy val parent = project in file(".") dependsOn (core, shapeless, scalaz) aggregate (core, shapeless, scalaz, tests) settings dontRelease

addCommandAlias("travis", ";clean;coverage;test;coverageReport;coverageAggregate")
