name := "ad4s"
version := "0.0.1-SNAPSHOT"

lazy val commonsSettings = Seq(
  scalaVersion := "2.12.7",
  resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  scalacOptions ++= Seq(
    "-encoding", "UTF-8", // source files are in UTF-8
    "-deprecation", // warn about use of deprecated APIs
    "-unchecked", // warn about unchecked type parameters
    "-feature", // warn about misused language features
    "-language:higherKinds", // allow higher kinded types without `import scala.language.higherKinds`
    "-language:implicitConversions",
    "-Xlint", // enable handy linter warnings
    //  "-Xfatal-warnings",     // turn compiler warnings into errors
    "-Ypartial-unification" // allow the compiler to unify type constructors of different arities
  ),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "1.2.0",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "org.scalatest" %% "scalatest" % "3.0.2" % Test,
    "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
    "org.typelevel" %% "cats-testkit" % "1.1.0" % Test, //or `cats-testkit` if you are using ScalaTest
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test
  ),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.0")
)
lazy val core = (project in file("ad4s-core"))
  .settings(commonsSettings)
  .settings(
    name := "ad4s-core",
    libraryDependencies ++= Seq(
      "com.github.mpilquist" %% "simulacrum" % "0.16.0",
      "org.typelevel" %% "spire" % "0.14.1"
    )
  )

lazy val breeze = (project in file("ad4s-breeze"))
  .settings(commonsSettings)
  .settings(
    name := "ad4s-breeze",
    libraryDependencies ++= Seq(
      "org.scalanlp" %% "breeze" % "0.13.2",
      "org.scalanlp" %% "breeze-natives" % "0.13.2",
    )
  ).dependsOn(core)

lazy val net = (project in file("ad4s-net"))
  .settings(commonsSettings)
  .settings(
    name := "ad4s-net",
    libraryDependencies ++= Seq(
    )
  ).dependsOn(core, breeze)

lazy val root = (project in file("."))
  .settings(commonsSettings)
  .aggregate(core, breeze, net)