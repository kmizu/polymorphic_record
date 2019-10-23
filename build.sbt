organization := "com.github.kmizu"

name := "polymorphic_record"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.10"

testOptions in Test += Tests.Argument("-oI")

scalacOptions ++= {
  Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")
}

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.7" % "test",
  "org.scalatest" %% "scalatest" %  "3.0.0"
)

assemblyJarName in assembly := "polymorphic_record.jar"

mainClass in assembly := Some("com.github.kmizu.polymorphic_record.Main")
