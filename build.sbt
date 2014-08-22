name := "shameless"

scalaVersion in ThisBuild := "2.11.0"

version := "1.0-SNAPSHOT"

libraryDependencies in ThisBuild ++= Seq(
  "com.chuusai"     %% "shapeless"        % "2.0.0",
  "org.scalaz"      %% "scalaz-core"      % "7.1.0-M7",
  "org.scalatest"   %  "scalatest_2.11"   % "2.1.3"             % "test",
  "org.scala-lang"  %  "scala-reflect"    % scalaVersion.value  % "provided",
  "org.scala-lang"  %  "scala-compiler"   % scalaVersion.value  % "test",
  "nl.grons"        %% "metrics-scala"    % "3.2.1_a2.3"
)

resolvers in ThisBuild  ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

//scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlog-implicits")

lazy val root = project.aggregate(core, testsHList, testsFTree)

lazy val core = (project in file(".")) settings (
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlog-implicits")
)

lazy val testsHList = (project in file("tests-hlist")) dependsOn core

lazy val testsFTree = (project in file("tests-ftree")) dependsOn core