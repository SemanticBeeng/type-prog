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
    "com.chuusai" % "shapeless_2.11" % "2.3.1",
    "org.scalaz" %% "scalaz-core" % "7.2.3"
  )
}

scalacOptions <<= scalaVersion map { v: String =>
  "-deprecation" :: "-unchecked" :: "-feature" :: "-language:postfixOps" :: "-language:implicitConversions" :: "-language:higherKinds" :: Nil
}

parallelExecution in Test := false
