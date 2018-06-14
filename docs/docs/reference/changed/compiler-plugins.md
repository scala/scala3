---
layout: doc-page
title: "Changes in Compiler Plugins"
---

Compiler plugins are supported by Dotty since 0.9. Compared to Scalac, there are
two notable changes:

- No more support for analyzer plugins
- Added support for research plugins

[Analyzer plugins][1] in Scalac are executed during type checking to change the
normal type checking. This is a nice feature for doing research, but for
production usage, a predictable and consistent type checker is more important.

For experiments and researches that rely on analyzer plugins in Scalac,
_research plugin_ can be used for the same purpose in Dotty. Research plugins
are more powerful than Scalac analyzer plugins as they enable plugin authors to
customize the whole compiler pipeline. That means, you can easily use your
customized typer to replace the standard typer, or roll your own parser for
your domain-specific language. Research plugins are only enabled for nightly or
snaphot releases of Dotty.

The common plugins that add new phases to the compiler pipeline are called
_standard plugins_ in Dotty. In terms of features, they are similar to
Scalac plugins, despite minor changes in the API.

## Physical Interface

Both research plugins and standard plugins share the same command line options
as Scalac plugins. You may manually specify a plugin as a compiler option as follows:

```shell
dotc -Xplugin:pluginA.jar -Xplugin:pluginB.jar Test.scala
```

The compiler will examine the jar provided, and look for a property file
`plugin.properties` in the root directory of the jar. The property file
specifies the fully qualified plugin class name. The format of a property file
looks like the following:

```
pluginClass=dividezero.DivideZero
```

The above is a change from Scalac, which depends on an XML file
`scalac-plugin.xml`. Starting from 1.1.5, SBT also supports Dotty compiler plugins:

```Scala
addCompilerPlugin("org.divbyzero" % "divbyzero" % "1.0")
```

## Standard Plugin

The following code example shows the template for a standard plugin:

```Scala
package dividezero

import dotty.tools.dotc._
import core._
import Contexts.Context
import plugins._
import Phases.Phase
import ast.tpd
import transform.{LinkAll, Pickler}

class DivideZero extends StandardPlugin {
  val name: String = "divideZero"
  override val description: String = "divide zero check"

  def init(options: List[String]): List[PluginPhase] = (new DivideZeroPhase) :: Nil
}

class DivideZeroPhase extends PluginPhase {
  val phaseName = "divideZero"

  override val runsAfter = Set(Pickler.name)
  override val runsBefore = Set(LinkAll.name)

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context): tpd.Tree = {
    // check whether divide by zero here
    tree
  }
}
```

As you can see from the code above, the plugin main class `DivideZero`
extends the trait `StandardPlugin`. It implements the method `init` which
takes the options for the plugin and return a list of `PluginPhase`s to be
inserted into the compilation pipeline.

The plugin `DivideZero` only adds one compiler phase, `DivideZeroPhase`,
to the compiler pipeline. The compiler phase has to extend the trait
`PluginPhase`. It also needs to tell the compiler the place where it wants to be
in the pipeline by specifying `runsAfter` and `runsBefore` relative to standard
compiler phases. Finally, it can transform the trees of interest by overriding
methods like `transformXXX`.

Usually a compiler plugin requires significant compiler knowledge in order to
maintain invariants of the compiler. It is a good practice to enable
the compiler option `-Ycheck:all` in the test set of your plugin.

## Research Plugin

Research plugins extend the trait `ResearchPlugin` as the following code shows:

```Scala
import dotty.tools.dotc._
import core._
import Contexts.Context
import plugins._
import Phases.Phase

class DummyResearchPlugin extends ResearchPlugin {
  val name: String = "dummy"
  override val description: String = "dummy research plugin"

  def init(options: List[String], phases: List[List[Phase]])(implicit ctx: Context): List[List[Phase]] =
    phases
}
```

Research plugins also define a method `init`, but the signature is different.
Research plugins receive options for the plugin and the whole compiler pipeline as parameters.
Usually, the `init` method replaces some standard phase of the compiler pipeline
with a custom phase, e.g. use a custom frontend. Finally, `init` returns the
updated compiler pipeline.

Note that research plugins are only enabled for nightly or snaphot release of Dotty.


[1]: https://github.com/scala/scala/blob/2.13.x/src/compiler/scala/tools/nsc/typechecker/AnalyzerPlugins.scala
