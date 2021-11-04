
val scala3Version = "3.1.0"

// library name
name := "scala-automata"

// library version
version := "0.2.1"

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

// add sonatype repository settings
// snapshot versions publish to sonatype snapshot repository
// other versions publish to sonatype staging repository
pomIncludeRepository := { _ => false }
val nexus = "https://s01.oss.sonatype.org/"
publishTo := {
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
publishMavenStyle := true

ThisBuild / versionScheme := Some("semver-spec")

// end of maven etc. publishing section
/////////////////////////////////////////////////////////////////

libraryDependencies += "org.typelevel" %% "paiges-core" % "0.4.2"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
libraryDependencies += "org.maraist" %% "scala-latex" % "2.0.0"
unmanagedSources / excludeFilter := ".#*"
Global / excludeLintKeys ++= Set(scalacOptions)
Compile / doc / scalacOptions ++= Seq(
  // "-groups",
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
