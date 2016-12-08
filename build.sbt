import Dependencies._

lazy val root = (project in file("."))
  .settings(
    inThisBuild(List(
      organization := "com.mogproject",
      scalaVersion := "2.12.0"
    )),
    name := "mog-core-scala",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      scalaCheck % Test
    ),
    initialCommands in console :=
      """
      import com.mogproject.mogami.core._
      """
  )
