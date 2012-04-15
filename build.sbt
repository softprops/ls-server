organization := "me.lessis"

name := "ls-server"

version := "0.1.1-SNAPSHOT"

resolvers ++= Seq(
  "coda" at "http://repo.codahale.com",
  "lessis" at "http://repo.lessis.me",
  "sona" at "http://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "me.lessis" %% "ls" % "0.1.2-RC2",
  "com.codahale" %% "jerkson" % "0.5.0",
  "net.databinder" %% "dispatch-http" % "0.8.6",
  "net.databinder" %% "dispatch-lift-json" % "0.8.5",
  "net.databinder" %% "unfiltered-netty-server" % "0.5.3",
  "com.mongodb.casbah" %% "casbah" % "2.1.5-1"
)

seq(coffeeSettings: _*)

seq(lessSettings: _*)

seq(heroicSettings: _*)

(resourceManaged in (Compile, CoffeeKeys.coffee)) <<= (
  resourceManaged in Compile) {
    _ / "www" / "js"
}
  
(resourceManaged in (Compile, LessKeys.less)) <<= (
  resourceManaged in Compile) {
    _ / "www" / "css"
}

(LessKeys.mini in (Compile, LessKeys.less)) := true
