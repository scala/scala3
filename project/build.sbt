// Used by VersionUtil to get gitHash and commitDate
libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "4.11.0.201803080745-r"

libraryDependencies += Dependencies.`jackson-databind`

// Configuration to publish build compilation to local and remote cache
ThisBuild / develocityConfiguration := {
  val isInsideCI = insideCI.value
  val config = develocityConfiguration.value
  val buildScan = config.buildScan
  val buildCache = config.buildCache
  config
    .withProjectId(ProjectId("scala3-build"))
    .withServer(config.server.withUrl(Some(url("https://develocity.scala-lang.org"))))
    .withBuildScan(buildScan.withPublishing(Publishing.onlyIf(_ => false)))
    .withBuildCache(
      buildCache
        .withLocal(buildCache.local.withEnabled(true).withStoreEnabled(true))
        .withRemote(buildCache.remote.withEnabled(true).withStoreEnabled(isInsideCI))
        .withRequireClean(!isInsideCI) // always cache inside CI
    )
}
