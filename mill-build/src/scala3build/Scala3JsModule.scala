package scala3build

import mill.*
import mill.scalajslib.*
import mill.scalajslib.api.*
import mill.scalalib.*
import org.scalajs.linker.interface.*

trait Scala3JsModule extends Scala3Module with ScalaJSModule {
  def scalaJSVersion = Versions.scalaJSVersion

  def enableWebAssembly: Boolean = false

  // Disable annoying "This is often an error due to a missing second colon (:) before the version" messages
  def resolvedDepsWarnNonPlatform = false

  def scalaJSConfig = Task.Anon {
    var config = super.scalaJSConfig()
      .withCheckIR(true)

    if (enableWebAssembly)
      config = config
        .withModuleKind(ModuleKind.ESModule)
        .withExperimentalUseWebAssembly(true)

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

  trait Scala3JsTests extends Scala3Tests with ScalaJSTests {
    def mvnDeps = Task {
      val baseDeps = super.mvnDeps().filter { dep =>
        val org = dep.dep.module.organization.value
        !org.contains("junit")
      }
      baseDeps ++ Seq(
        mvn"org.scala-js:scalajs-junit-test-runtime_2.13:${Versions.scalaJSVersion}"
      )
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
        .filter { dep =>
          val org = dep.dep.module.organization.value
          !org.contains("junit")
        }
    }
    def testFramework = "com.novocode.junit.JUnitFramework"
  }
}
