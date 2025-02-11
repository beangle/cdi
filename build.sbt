import org.beangle.parent.Dependencies.*
import org.beangle.parent.Settings.*
import sbt.Keys.libraryDependencies
import sbt.url

ThisBuild / organization := "org.beangle.cdi"
ThisBuild / version := "0.7.2"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/beangle/cdi"),
    "scm:git@github.com:beangle/cdi.git"
  )
)

ThisBuild / developers := List(
  Developer(
    id = "chaostone",
    name = "Tihua Duan",
    email = "duantihua@gmail.com",
    url = url("http://github.com/duantihua")
  )
)

ThisBuild / description := "The Beangle CDI Library"
ThisBuild / homepage := Some(url("https://beangle.github.io/cdi/index.html"))

val beangle_commons = "org.beangle.commons" % "beangle-commons" % "5.6.26"

lazy val root = (project in file("."))
  .settings(
    name := "beangle-cdi",
    common,
    libraryDependencies ++= Seq(beangle_commons, scalaxml),
    libraryDependencies ++= Seq(logback_classic % "test", scalatest),
    libraryDependencies += spring_context % "optional",
    libraryDependencies += spring_beans % "optional"
  )

