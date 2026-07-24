package scala3build

import mill.*
import mill.scalajslib.*
import mill.scalajslib.api.*
import mill.scalajslib.config.ScalaJSConfigModule
import mill.scalalib.*
import org.scalajs.ir.ScalaJSVersions
import org.scalajs.linker.{interface => sjs}

/**
 * Tasks for Scala.js modules of the Scala 3 build
 *
 * This configures the Scala.js linker and the JS environment, and Scala.js' JUnit 4 in the tests.
 */
trait Scala3JsModule extends Scala3Module with ScalaJSConfigModule { outer =>
  def enableWebAssembly: Boolean = false

  // Disable "This is often an error due to a missing second colon (:) before the version" messages
  def resolvedDepsWarnNonPlatform = false

  def scalaJSConfig = Task.Anon {
    var config = super.scalaJSConfig()
      .withCheckIR(true)

    if (enableWebAssembly)
      config = config
        .withModuleKind(sjs.ModuleKind.ESModule)
        .withESFeatures(_.withUseWebAssembly(true))
        .withWasmFeatures(_.withUseJSPI(true)) // for async/await support in WebAssembly

    config
  }

  def jsEnvConfig = Task {
    val config = JsEnvConfig.NodeJs()
    if (enableWebAssembly)
      config.copy(
        args = List(
          "--experimental-wasm-exnref",
          "--experimental-wasm-imported-strings", // Original sbt build comment: for JS string builtins
          "--experimental-wasm-jspi", // Original sbt build comment: for JSPI, used by async/await
        )
      )
    else
      config
  }

  def mandatoryMvnDeps = Task {
    super.mandatoryMvnDeps()
      .filter { dep =>
        val (org, name) = (dep.dep.module.organization.value, dep.dep.module.name.value)
        org != "org.scala-lang" || !name.startsWith("scala3-library")
      }
      .map { dep =>
        dep.exclude(("org.scala-lang", "scala-library"))
      }
  }

  trait Scala3JsTests extends Scala3Tests with ScalaJSConfigTests {
    def resolvedDepsWarnNonPlatform = false

    override def discoverTestsWithZinc = true

    def mandatoryMvnDeps = Task {
      val baseDeps =
        super.mandatoryMvnDeps().filter { dep =>
          val org = dep.dep.module.organization.value
          val name = dep.dep.module.name.value
          (org, name) != ("com.github.sbt", "junit-interface")
        } ++
        Seq(
          mvn"org.scala-js:scalajs-junit-test-runtime_2.13:${ScalaJSVersions.current}"
            .exclude(("org.scala-lang", "scala-library"))
        )
      baseDeps
        .filter { dep =>
          val (org, name) = (dep.dep.module.organization.value, dep.dep.module.name.value)
          org != "org.scala-lang" || !name.startsWith("scala3-library")
        }
        .map { dep =>
          dep.exclude(("org.scala-lang", "scala-library"))
        }
        .filter { dep =>
          val org = dep.dep.module.organization.value
          !org.contains("junit")
        }
    }
  }
}
