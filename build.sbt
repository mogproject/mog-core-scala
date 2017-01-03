lazy val root = (project in file("."))
  .aggregate(mogCoreJVM, mogCoreJS)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val mogCore = crossProject.in(file("."))
  .settings(
    inThisBuild(List(
      organization := "com.mogproject",
      scalaVersion := "2.12.0"
    )),
    name := "mog-core-scala",
    version := "0.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.0.1" % Test,
      "org.scalacheck" %%% "scalacheck" % "1.13.4" % Test
    )
  )
  .jvmSettings(
    scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation"),
    parallelExecution in Test := false,
    initialCommands in console in Test :=
      """
      import com.mogproject.mogami.core._
      import com.mogproject.mogami.core.Player.{BLACK, WHITE}
      import com.mogproject.mogami.core.Ptype._
      import com.mogproject.mogami.core.SquareConstant._
      import com.mogproject.mogami.core.PieceConstant._
      import com.mogproject.mogami.core.State.PromotionFlag._
      """
  )
  .jsSettings(
    // Add JS-specific settings here
  )

lazy val mogCoreJVM = mogCore.jvm
lazy val mogCoreJS = mogCore.js