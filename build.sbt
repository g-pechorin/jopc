
name := "jopc"
organization := "com.peterlavalle"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.2.0" % Test,
	"org.json" % "json" % "20200518",
)
 

testOptions in Test ++= Seq(
	Tests.Argument("-oD", "-u", "target/test-reports")
)