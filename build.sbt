
/// ====
// monorepo config block

import sbt.Def
import java.io.File

val hgRoot: File = {
	var root = file("").getAbsoluteFile

	while (!(root / "sbt.bin/scala.conf").exists())
		root = root.getAbsoluteFile.getParentFile.getAbsoluteFile

	root
}

def conf: String => String = {
	import com.typesafe.config.ConfigFactory

	(key: String) =>
		ConfigFactory.parseFile(
			hgRoot / "sbt.bin/scala.conf"
		).getString(key)
}
// end of monorepo config block

organization := "com.peterlavalle"
scalaVersion := conf("scala.version")
scalacOptions ++= conf("scala.options").split("/").toSeq

resolvers += Classpaths.typesafeReleases
resolvers += Resolver.mavenCentral
resolvers += Resolver.jcenterRepo
resolvers += "jitpack" at "https://jitpack.io"

// end of standard stuff
/// ---

name := "jopc"

Test / unmanagedResourceDirectories ++= { (Test / unmanagedSourceDirectories).value }

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.2.0" % Test,
	"org.json" % "json" % "20200518",
)
