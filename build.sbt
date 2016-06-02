name := "type-prog"

organization := "com.joescii"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  "sonatype-snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "sonatype-releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= {
  Seq(
    "org.scalatest"  %% "scalatest"   % "2.2.6"    % "test",
    //"com.chuusai"    % "shapeless"    % "2.9.2" % "test" cross CrossVersion.full,
    "com.chuusai" % "shapeless_2.11" % "2.3.1"
//    "org.scalacheck" %% "scalacheck"  % "1.10.1" % "test"
  )
}

scalacOptions <<= scalaVersion map { v: String =>
  "-deprecation" :: "-unchecked" :: "-feature" :: "-language:postfixOps" :: "-language:implicitConversions" :: "-language:higherKinds" :: Nil
}

parallelExecution in Test := false
