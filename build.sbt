scalaVersion := "2.9.2"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xfatal-warnings",
                      "-encoding", "us-ascii")

libraryDependencies +=
  "org.nlogo" % "NetLogo" % "5.0.1" from
    "http://ccl.northwestern.edu/netlogo/5.0.1/NetLogo.jar"

artifactName := { (_, _, _) => "inf.jar" }

packageOptions := Seq(
  Package.ManifestAttributes(
    ("Extension-Name", "inf"),
    ("Class-Manager", "InfExtension"),
    ("NetLogo-Extension-API-Version", "5.0")))

packageBin in Compile <<= (packageBin in Compile, baseDirectory, streams) map {
  (jar, base, s) =>
    IO.copyFile(jar, base / "inf.jar")
    Process("pack200 --modification-time=latest --effort=9 --strip-debug " +
            "--no-keep-file-order --unknown-attribute=strip " +
            "inf.jar.pack.gz inf.jar").!!
    if(Process("git diff --quiet --exit-code HEAD").! == 0) {
      Process("git archive -o inf.zip --prefix=inf/ HEAD").!!
      IO.createDirectory(base / "inf")
      IO.copyFile(base / "inf.jar", base / "inf" / "inf.jar")
      IO.copyFile(base / "inf.jar.pack.gz", base / "inf" / "inf.jar.pack.gz")
      Process("zip inf.zip inf/inf.jar inf/inf.jar.pack.gz").!!
      IO.delete(base / "inf")
    }
    else {
      s.log.warn("working tree not clean; no zip archive made")
      IO.delete(base / "inf.zip")
    }
    jar
  }

cleanFiles <++= baseDirectory { base =>
  Seq(base / "inf.jar",
      base / "inf.jar.pack.gz",
      base / "inf.zip") }

