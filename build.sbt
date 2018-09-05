organization  := "org.babysitter"
version       := "0.1.0"
scalaVersion  := "2.11.6"
scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val sprayV = "1.3.1"
  Seq(
    "org.scala-lang" % "scala-library" % "2.11.6",
    "io.spray" %% "spray-json" % "1.3.2",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test"
  )
}
resolvers += Resolver.typesafeIvyRepo("releases")
