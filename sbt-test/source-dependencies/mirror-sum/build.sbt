scalaVersion := sys.props("plugin.scalaVersion")
// Use more precise invalidation, otherwise the reference to `Sum` in
// Test.scala is enough to invalidate it when a child is added.
ThisBuild / incOptions ~= { _.withUseOptimizedSealed(true) }
