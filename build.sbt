
val scala3Version = "3.0.2-RC1"

// library name
name := "scala-automata"

// library version
version := "0.2.0"

/////////////////////////////////////////////////////////////////
// begin maven etc. publishing information

// groupId, SCM, license information
organization := "org.maraist"
homepage := Some(url("https://github.com/jphmrst/scala-automata"))
scmInfo := Some(ScmInfo(
  url("https://github.com/jphmrst/scala-automata"),
  "git@github.com:jphmrst/scala-automata.git"))
developers := List(Developer(
  "jphmrst", "jphmrst", "via-github@maraist.org",
  url("https://maraist.org/work/")))
licenses += (
  "Educational",
  url("https://github.com/jphmrst/scala-automata/blob/master/LICENSE.txt"))
publishMavenStyle := true

// disable publish with scala version, otherwise artifact name will
// include scala version
// e.g cassper_2.11
crossPaths := false

// add sonatype repository settings
// snapshot versions publish to sonatype snapshot repository
// other versions publish to sonatype staging repository
publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

ThisBuild / versionScheme := Some("semver-spec")

// end of maven etc. publishing section
/////////////////////////////////////////////////////////////////

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
libraryDependencies += "org.maraist" %% "scala-latex" % "1.1.1"
unmanagedSources / excludeFilter := ".#*"
Global / excludeLintKeys ++= Set(scalacOptions)
Compile / doc / scalacOptions ++= Seq(
  "-groups",
  "-doc-root-content", "src/root.scaladoc"
)

lazy val main = project
  .in(file("."))
  .settings(
    run / fork := true,
    run / baseDirectory := file("./samples/"),
    scalaVersion := scala3Version,
    compile / watchTriggers += baseDirectory.value.toGlob / "build.sbt",
    unmanagedSources / excludeFilter := ".#*",
    scalacOptions ++= Seq( "-source:future-migration" ),
  )
