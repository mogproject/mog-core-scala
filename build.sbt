lazy val root = (project in file("."))
  .aggregate(mogCoreJVM, mogCoreJS)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val mogCore = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(
    inThisBuild(List(
      organization := "com.mogproject",
      scalaVersion := "2.13.2",
    )),
    name := "mog-core-scala",
    version := "0.2-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.1.2" % Test,
      "org.scalacheck" %%% "scalacheck" % "1.14.3" % Test,
      "org.scalatestplus" %%% "scalacheck-1-14" % "3.1.2.0" % Test
    ),
    scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation"),
    parallelExecution in Test := false,
    crossPaths := false,
    scalaJSUseMainModuleInitializer := true
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
      import com.mogproject.mogami.core.state.StateCache.Implicits._
      import com.mogproject.mogami.mate.MateSolver
      """
  )
  .jsSettings(
    //
  )

lazy val mogCoreJVM = mogCore.jvm
lazy val mogCoreJS = mogCore.js
