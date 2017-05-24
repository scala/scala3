// Include the sources of the sbt-dotty plugin in the project build,
// so that we can use the current in-development version of the plugin
// in our build instead of a released version.

unmanagedSourceDirectories in Compile += baseDirectory.value / "../sbt-dotty/src"
