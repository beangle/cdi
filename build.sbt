import Dependencies._
import BuildSettings._
import sbt.url

ThisBuild / organization := "org.beangle.cdi"
ThisBuild / version := "0.3.2"

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

lazy val root = (project in file("."))
  .settings()
  .aggregate(api,spring)

lazy val api = (project in file("api"))
  .settings(
    name := "beangle-cdi-api",
    commonSettings,
    libraryDependencies ++= commonDeps
  )

lazy val spring = (project in file("spring"))
  .settings(
    name := "beangle-cdi-spring",
    commonSettings,
    libraryDependencies ++= (commonDeps ++ Seq(servletapi,springContext,springBeans,scalaxml,h2))
  ).dependsOn(api)

publish / skip := true
