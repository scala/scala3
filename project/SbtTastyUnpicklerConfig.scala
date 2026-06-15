package dotty.tools.sbtplugin

import dotty.tools.tasty.UnpicklerConfig

/** TASTy reader config for the meta-build, aligned with [[Versions.expectedTastyVersion]].
 *
 *  Uses sbt's bundled tasty-core for parsing, but supplies version constants from this
 *  build's reference compiler so we can read TASTy emitted by `scala-library-nonbootstrapped`.
 */
object SbtTastyUnpicklerConfig {
  private val (major, minor, experimental) =
    Versions.expectedTastyVersion.split("\\.|-").take(4) match {
      case Array("28", minor)                      => (28, minor.toInt, 0)
      case Array("28", minor, "experimental", exp) => (28, minor.toInt, exp.toInt)
      case _ =>
        sys.error(s"Invalid TASTy version string: ${Versions.expectedTastyVersion}")
    }

  val unpicklerConfig: UnpicklerConfig =
    new UnpicklerConfig with UnpicklerConfig.Generic {
      override def majorVersion: Int = major
      override def minorVersion: Int = minor
      override def experimentalVersion: Int = experimental
    }
}
