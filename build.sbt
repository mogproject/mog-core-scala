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
    scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation"),
    initialCommands in console in Test :=
      """
      import com.mogproject.mogami.core._
      import com.mogproject.mogami.core.Player.{BLACK, WHITE}
      import com.mogproject.mogami.core.Ptype._
      import com.mogproject.mogami.core.SquareConstant._
      import com.mogproject.mogami.core.PieceConstant._
      import com.mogproject.mogami.core.Square.HAND
      import com.mogproject.mogami.core.State.PromotionFlag._
      """,
    parallelExecution in Test := false
  )
