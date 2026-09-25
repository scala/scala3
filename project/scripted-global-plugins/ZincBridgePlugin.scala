import sbt.*
import sbt.Keys.*

/** Makes scripted sbt use the publishLocal `scala3-sbt-bridge` jar.
 *
 *  sbt 2 requests that jar as an optional `zinc-tool` dependency. Optional
 *  resolution does not see `~/.ivy2/local`, so a just-published bridge is
 *  missed and sbt compiles the bridge sources instead. That compilation
 *  emits only `META-INF/services/xsbti.compile.CompilerInterface2`, and the
 *  next compile fails with "Provider dotty.tools.xsbt.CompilerBridge not found".
 *  A non-optional dependency on the same module resolves the published jar.
 */
object ZincBridgePlugin extends AutoPlugin {
  override def trigger = allRequirements
  override def requires = plugins.JvmPlugin
  override def projectSettings = Seq(
    libraryDependencies ++= {
      val sv = scalaVersion.value
      val meta = thisProject.value.id.endsWith("-build")
      if !meta && sv.startsWith("3.") then
        Seq("org.scala-lang" % "scala3-sbt-bridge" % sv % "zinc-tool")
      else Nil
    },
  )
}
