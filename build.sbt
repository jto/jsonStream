name := "jsonStream"

version := "1.0-SNAPSHOT"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "org.scalaz.stream" %% "scalaz-stream" % "0.3"
)

play.Project.playScalaSettings
