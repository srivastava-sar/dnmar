name := "DNMAR"

version := "0.1"

libraryDependencies  ++= Seq(
            // other dependencies here
            "org.scalala" %% "scalala" % "1.0.0.RC3-SNAPSHOT",
	    "org.clapper" %% "argot" % "0.4",
	    "org.scalacheck" %% "scalacheck" % "1.9" % "test"
)

resolvers ++= Seq(
            // other resolvers here
            "Scala Tools Snapshots" at "https://oss.sonatype.org/content/groups/scala-tools/repo-snapshots/",
            "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"
)

scalaVersion := "2.9.2"