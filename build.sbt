name := "cats"

version := "0.1"

scalaVersion := "2.13.0"

val catsVersion = "2.0.0-RC1"
scalacOptions ++= Seq(
  "-Ywarn-value-discard",
  "-Xfatal-warnings"
)
scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-language:higherKinds",
  "-Ypartial-unification")

// https://mvnrepository.com/artifact/org.typelevel/cats-core
libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
libraryDependencies += "org.typelevel" %% "cats-effect" % catsVersion withSources() withJavadoc()

libraryDependencies += "org.typelevel" %% "cats-free" % catsVersion
libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"


