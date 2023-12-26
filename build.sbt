val scala3Version = "3.3.1"

ThisBuild / scalaVersion := scala3Version
ThisBuild / scalacOptions += "-Ykind-projector:underscores"
resolvers += Resolver.mavenLocal
lazy val root = project
  .in(file("."))
  .settings(
    name := "ModestUI",
    version := "0.1.0-SNAPSHOT",


    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "io.github.humbleui" % "jwm" % "0.0.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "skija-windows-x64",
      "skija-linux-x64",
     // "skija-linux-arm64",
      "skija-macos-x64",
      "skija-macos-arm64"
    ).map("io.github.humbleui" % _ % "0.116.2"),
    libraryDependencies += "io.github.humbleui" % "types" % "0.2.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.2",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0",
    libraryDependencies += "co.fs2" %% "fs2-core" % "3.9.3",
    libraryDependencies += "org.typelevel" %% "shapeless3-deriving" % "3.0.1",
  )

lazy val sample = project
  .in(file("sample"))
  .dependsOn(root)
  .settings(
    name := "ModestUI-Sample",
    Compile / run / fork := true
  )
