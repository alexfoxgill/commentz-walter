name := "commentz-walter"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.2" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")