scalaVersion in ThisBuild := "2.12.4"

lazy val fastparse =
  "com.lihaoyi" %% "fastparse" % "1.0.0"

lazy val catsCore =
  "org.typelevel" %% "cats-core" % "1.1.0"

lazy val catsFree =
  "org.typelevel" %% "cats-free" % "1.1.0"

lazy val direct =
  project.in(file("direct"))

lazy val standalone =
  project.in(file("standalone"))
    .settings(libraryDependencies ++= Seq(catsCore, fastparse))

lazy val reified =
  project.in(file("reified"))
    .settings(libraryDependencies ++= Seq(catsCore))

lazy val runtimeErrors =
  project.in(file("runtimeErrors"))
    .settings(libraryDependencies ++= Seq(catsCore))

lazy val typedInfix =
  project.in(file("typedInfix"))
    .settings(libraryDependencies ++= Seq(catsCore))

lazy val exprMonad =
  project.in(file("exprMonad"))
    .settings(libraryDependencies ++= Seq(catsCore))

lazy val freeMonad =
  project.in(file("freeMonad"))
    .settings(libraryDependencies ++= Seq(catsCore, catsFree))

lazy val root =
  project.in(file("."))
    .aggregate(
      direct,
      standalone,
      reified,
      runtimeErrors,
      typedInfix,
      exprMonad,
      freeMonad,
    )
