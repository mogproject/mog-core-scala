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
    ),
    scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation"),
    parallelExecution in Test := false
  )
  .jvmSettings(
    initialCommands in console in Test :=
      """
      import com.mogproject.mogami._
      import com.mogproject.mogami.core.Player.{BLACK, WHITE}
      import com.mogproject.mogami.core.Ptype._
      import com.mogproject.mogami.core.SquareConstant._
      import com.mogproject.mogami.core.PieceConstant._
      import com.mogproject.mogami.core.state.StateConstant._
      import com.mogproject.mogami.core.state.State.PromotionFlag._
      """
  )
  .jsSettings(
    //
  )

lazy val mogCoreJVM = mogCore.jvm
lazy val mogCoreJS = mogCore.js