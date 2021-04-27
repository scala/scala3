package scala.tasty.inspector

import scala.quoted._
import scala.quoted.runtime.impl.QuotesImpl

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.Driver
import dotty.tools.dotc.Run
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.fromtasty._
import dotty.tools.dotc.util.ClasspathFromClassloader
import dotty.tools.dotc.CompilationUnit
import dotty.tools.unsupported
import dotty.tools.dotc.report

import java.io.File.pathSeparator

trait Inspector:

  /** Inspect all TASTy files using `Quotes` reflect API.
   *
   *  Note: Within this method `quotes.reflect.SourceFile.current` will not work, hence the explicit source paths.
   *
   *  @param tastys List of `Tasty` containing `.tasty`file path and AST
   */
  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit

end Inspector
