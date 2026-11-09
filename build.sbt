scalaVersion := "3.3.6"
name := "TemporaryCleaner"
version := "1.0"
Compile / run / fork := true
Compile / run / connectInput := true
Compile / run / mainClass := Some("cleaner.Main")
scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-feature", "-unchecked")
