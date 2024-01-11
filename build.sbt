lazy val root =
  project
    .in(file("."))
    .settings( scalaVersion := "3.2.2"
             , name         := "aoc-2016"
             , version      := "0.1.0"
             , libraryDependencies ++= Seq(
                 "org.scalatest"  %% "scalatest"  % "3.2.17" % "test"
               )
             )

scalacOptions ++= Seq(       
  "-encoding", "utf8",        
  "-feature",                 
  "-language:implicitConversions",
  "-language:existentials",
  "-unchecked",
  "-Werror",
  "-deprecation",
//  "-explain",
)
