package versionhelpers

object DottyVersion {
  val isRelease = sys.env.get("RELEASEBUILD") == Some("yes")

  val referenceVersion = "3.4.2-RC1"

  val baseVersion = "3.5.1-RC1"

  val dottyVersion = {
    def isNightly = sys.env.get("NIGHTLYBUILD") == Some("yes")
    if (isRelease)
      baseVersion
    else if (isNightly)
      baseVersion + "-bin-" + VersionUtil.commitDate + "-" + VersionUtil.gitHash + "-NIGHTLY"
    else
      baseVersion + "-bin-SNAPSHOT"
  }
  val dottyNonBootstrappedVersion = {
    // Make sure sbt always computes the scalaBinaryVersion correctly
    val bin = if (!dottyVersion.contains("-bin")) "-bin" else ""
    dottyVersion + bin + "-nonbootstrapped"
  }
}
