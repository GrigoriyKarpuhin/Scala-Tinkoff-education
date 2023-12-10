import sbt.*

object Dependencies {
  val testTest = Seq(
    "org.scalactic" %% "scalactic" % "3.2.17",
    "org.scalatest" %% "scalatest" % "3.2.17" % "test"
  )
}
