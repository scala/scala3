import language.`3.7`
import language.experimental.namedTuples
import language.experimental.collectionLiterals

case class BuildDescription(
  declarationMap: Boolean = false,
  esModuleInterop: Boolean = true,
  baseUrl: String = ".",
  rootDir: String = "",
  declaration: Boolean = false,
  outDir: String = ".",
  deps: Seq[Dep] = [junitInterface, commonsIo],
  plugins: Seq[Plugin]  = [],
  aliases: Seq[String] = [],
  moduleResolution: String = "",
  module: String = "",
  target: String = "",
  other: String = ""
)

case class Plugin(
  transform: String,
  afterDeclarations: Boolean = false
)

val b1: BuildDescription = (
  declarationMap = true,
  esModuleInterop = true,
  baseUrl = ".",
  rootDir = "typescript",
  declaration = true,
  outDir = pubBundledOut,
  deps = [junitInterface, commonsIo],
  plugins  = [
    ( transform = "typescript-transform-paths" ),
    ( transform = "typescript-transform-paths",
      afterDeclarations = true
    )
  ],
  aliases = ["someValue", "some-value", "a value"],
  moduleResolution = "node",
  module = "CommonJS",
  target = "ES2020"
)

val b2: BuildDescription = (
  declarationMap = true,
  baseUrl = ".",
  rootDir = "typescript",
  outDir = pubBundledOut,
  plugins  = [
    ( transform = "typescript-transform-paths" ),
    ( transform = "typescript-transform-paths",
      afterDeclarations = true
    )
  ],
  moduleResolution = "node",
  target = "ES2020"
)

val b3: BuildDescription = ()

@main def Test =
  println(b1)
  println(b2)
  println(b3)

class Dep
val junitInterface = Dep()
val commonsIo = Dep()

val pubBundledOut = ""
