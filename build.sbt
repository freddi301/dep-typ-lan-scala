name := "dep-typ-lan"

version := "0.1"

scalaVersion := "2.13.5"

enablePlugins(ScalaJSPlugin)

name := "Scala.js Tutorial"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"

jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()