import org.beangle.parent.Dependencies._
import org.beangle.parent.Settings._
import sbt.url

ThisBuild / organization := "org.beangle.cdi"
ThisBuild / version := "0.3.4"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/beangle/cdi"),
    "scm:git@github.com:beangle/cdi.git"
  )
)

ThisBuild / developers := List(
  Developer(
    id    = "chaostone",
    name  = "Tihua Duan",
    email = "duantihua@gmail.com",
    url   = url("http://github.com/duantihua")
  )
)

ThisBuild / description := "The Beangle CDI Library"
ThisBuild / homepage := Some(url("https://beangle.github.io/spring/index.html"))

val beangle_commons_core = "org.beangle.commons" %% "beangle-commons-core" % "5.2.9"
val commonDeps = Seq(beangle_commons_core, logback_classic, logback_core, scalatest)

lazy val root = (project in file("."))
  .settings()
  .aggregate(api,spring)

lazy val api = (project in file("api"))
  .settings(
    name := "beangle-cdi-api",
    common,
    libraryDependencies ++= commonDeps
  )

lazy val spring = (project in file("spring"))
  .settings(
    name := "beangle-cdi-spring",
    common,
    libraryDependencies ++= (commonDeps ++ Seq(servletapi,spring_context,spring_beans,scalaxml,h2))
  ).dependsOn(api)

publish / skip := true
