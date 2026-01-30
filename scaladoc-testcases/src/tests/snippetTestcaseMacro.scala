package tests.snippetTestcaseMacro

import scala.quoted.*

/** Regression test for https://github.com/scala/scala3/issues/24946
  * Scaladoc snippet compilation fails when the snippet calls a macro
  * defined in the same compilation unit.
  */
object MacroLib:
  inline def hello: String = ${ helloImpl }

  def helloImpl(using Quotes): Expr[String] = Expr("Hello")

class SnippetTestcaseMacro:
  /**
    * SNIPPET(OUTERLINEOFFSET:17,OUTERCOLUMNOFFSET:6,INNERLINEOFFSET:4,INNERCOLUMNOFFSET:2)
    * ```scala sc:compile
    * MacroLib.hello
    * ```
    */
  def a = 3
