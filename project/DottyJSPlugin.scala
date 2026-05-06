package dotty.tools.sbtplugin

import sbt.*
import sbt.Keys.*

import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

import org.scalajs.linker.interface.{ESVersion, StandardConfig}

import org.scalajs.jsenv.nodejs.NodeJSEnv

object DottyJSPlugin extends AutoPlugin {

  object autoImport {
    val switchToESModules: StandardConfig => StandardConfig =
      config => config.withModuleKind(ModuleKind.ESModule)

    val switchToLatestESVersion: StandardConfig => StandardConfig =
      config => config.withESFeatures(_.withESVersion(ESVersion.ES2021))

    val enableWebAssembly: SettingKey[Boolean] =
      settingKey("enable all the configuration items required for WebAssembly")
  }

  import autoImport._

  val writePackageJSON = taskKey[Unit](
      "Write package.json to configure module type for Node.js")

  override def requires: Plugins = ScalaJSPlugin

  override def globalSettings: Seq[Setting[_]] = Def.settings(
    enableWebAssembly := false,
  )

  override def projectSettings: Seq[Setting[_]] = Def.settings(

    /* #11709 Remove the dependency on scala3-library that ScalaJSPlugin adds.
     * Instead, in this build, we use `.dependsOn` relationships to depend on
     * the appropriate, locally-defined, scala3-library-bootstrappedJS.
     */
    libraryDependencies ~= {
      _.filter(!_.name.startsWith("scala3-library_sjs1"))
    },

    // Replace the JVM JUnit dependency by the Scala.js one
    libraryDependencies ~= {
      _.filter(!_.name.startsWith("junit-interface"))
    },
    libraryDependencies +=
      ("org.scala-js" %% "scalajs-junit-test-runtime" % scalaJSVersion  % "test").cross(CrossVersion.for3Use2_13),

    // Typecheck the Scala.js IR found on the classpath
    scalaJSLinkerConfig ~= (_.withCheckIR(true)),

    // Maybe configure WebAssembly
    scalaJSLinkerConfig := {
      val prev = scalaJSLinkerConfig.value
      if (enableWebAssembly.value) {
        prev
          .withModuleKind(ModuleKind.ESModule)
          .withExperimentalUseWebAssembly(true)
      } else {
        prev
      }
    },
    jsEnv := {
      val baseConfig = NodeJSEnv.Config()
      val config = if (enableWebAssembly.value) {
        baseConfig.withArgs(List(
          "--experimental-wasm-exnref",
          "--experimental-wasm-imported-strings", // for JS string builtins
          "--experimental-wasm-jspi", // for JSPI, used by async/await
        ))
      } else {
        baseConfig
      }
      new NodeJSEnv(config)
    },

    Compile / jsEnvInput := (Compile / jsEnvInput).dependsOn(writePackageJSON).value,
    Test / jsEnvInput := (Test / jsEnvInput).dependsOn(writePackageJSON).value,

    writePackageJSON := {
      val packageType = scalaJSLinkerConfig.value.moduleKind match {
        case ModuleKind.NoModule       => "commonjs"
        case ModuleKind.CommonJSModule => "commonjs"
        case ModuleKind.ESModule       => "module"
      }

      val path = target.value / "package.json"

      IO.write(path, s"""{"type": "$packageType"}\n""")
    },
  )
}
