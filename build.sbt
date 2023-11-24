val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "ModestUI",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "io.github.humbleui" % "jwm" % "0.4.15",
    libraryDependencies ++= Seq(
      "skija-windows-x64",
      "skija-linux-x64",
     // "skija-linux-arm64",
      "skija-macos-x64",
      "skija-macos-arm64"
    ).map("io.github.humbleui" % _ % "0.116.1"),
    libraryDependencies += "io.github.humbleui" % "types" % "0.2.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.2",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"
  )