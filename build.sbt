lazy val fastparse =
  "com.lihaoyi" %% "fastparse" % "1.0.0"

lazy val direct =
  project.in(file("direct"))

lazy val standalone =
  project.in(file("standalone"))
    .settings(libraryDependencies += fastparse)

lazy val reified =
  project.in(file("reified"))

lazy val runtimeErrors =
  project.in(file("runtimeErrors"))

lazy val typedInfix =
  project.in(file("typedInfix"))

lazy val root =
  project.in(file("."))
    .aggregate(
      direct,
      standalone,
      reified,
      runtimeErrors,
      typedInfix
    )
