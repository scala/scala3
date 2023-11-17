package dotty.tools
package dotc
package parsing

import util.SourceFile
import core.*
import Contexts.*
import Parsers.*


/** <p>Performs the following context-free rewritings:</p>
 *  <ol>
 *    <li>
 *      Places all pattern variables in Bind nodes. In a pattern, for
 *      identifiers <code>x</code>:<pre>
 *                 x  => x @ _
 *               x:T  => x @ (_ : T)</pre>
 *    </li>
 *    <li>Removes pattern definitions (PatDef's) as follows:
 *      If pattern is a simple (typed) identifier:<pre>
 *        <b>val</b> x = e     ==>  <b>val</b> x = e
 *        <b>val</b> x: T = e  ==>  <b>val</b> x: T = e</pre>
 *
 *      if there are no variables in pattern<pre>
 *        <b>val</b> p = e  ==>  e match (case p => ())</pre>
 *
 *      if there is exactly one variable in pattern<pre>
 *        <b>val</b> x_1 = e <b>match</b> (case p => (x_1))</pre>
 *
 *      if there is more than one variable in pattern<pre>
 *        <b>val</b> p = e  ==>  <b>private synthetic val</b> t$ = e <b>match</b> (case p => (x_1, ..., x_N))
 *                        <b>val</b> x_1 = t$._1
 *                        ...
 *                        <b>val</b> x_N = t$._N</pre>
 *    </li>
 *    <li>
 *       Removes function types as follows:<pre>
 *        (argtpes) => restpe   ==>   scala.Function_n[argtpes, restpe]</pre>
 *    </li>
 *    <li>
 *      Wraps naked case definitions in a match as follows:<pre>
 *        { cases }   ==>   (x => x.match {cases})<span style="font-family:normal;">, except when already argument to match</span></pre>
 *    </li>
 *  </ol>
 */
object ScriptParsers {

  import ast.untpd.*

  class ScriptParser(source: SourceFile)(using Context) extends Parser(source) {

    /** This is the parse entry point for code which is not self-contained, e.g.
     *  a script which is a series of template statements.  They will be
     *  swaddled in Trees until the AST is equivalent to the one returned
     *  by compilationUnit().
     */
    override def parse(): Tree = unsupported("parse")
  }
}

    /* TODO: reinstantiate

      val stmts = templateStatSeq(false)._2
      accept(EOF)

      def mainModuleName = ctx.settings.script.value

      /** If there is only a single object template in the file and it has a
       *  suitable main method, we will use it rather than building another object
       *  around it.  Since objects are loaded lazily the whole script would have
       *  been a no-op, so we're not taking much liberty.
       */
      def searchForMain(): Option[Tree] = {
        /** Have to be fairly liberal about what constitutes a main method since
         *  nothing has been typed yet - for instance we can't assume the parameter
         *  type will look exactly like "Array[String]" as it could have been renamed
         *  via import, etc.
         */
        def isMainMethod(t: Tree) = t match {
          case DefDef(_, nme.main, Nil, List(_), _, _)  => true
          case _                                        => false
        }
        /** For now we require there only be one top level object. */
        var seenModule = false
        val newStmts = stmts collect {
          case t @ Import(_, _) => t
          case md @ ModuleDef(mods, name, template)
            if !seenModule && (template.body exists isMainMethod) =>
            seenModule = true
            /** This slightly hacky situation arises because we have no way to communicate
             *  back to the scriptrunner what the name of the program is.  Even if we were
             *  willing to take the sketchy route of settings.script.value = progName, that
             *  does not work when using fsc.  And to find out in advance would impose a
             *  whole additional parse.  So instead, if the actual object's name differs from
             *  what the script is expecting, we transform it to match.
             */
            md.derivedModuleDef(mods, mainModuleName.toTermName, template)
          case _ =>
            /** If we see anything but the above, fail. */
            return None
        }
        Some(makePackaging(0, emptyPkg, newStmts))
      }

      if (mainModuleName == ScriptRunner.defaultScriptMain)
        searchForMain() foreach { return _ }

      /** Here we are building an AST representing the following source fiction,
       *  where <moduleName> is from -Xscript (defaults to "Main") and <stmts> are
       *  the result of parsing the script file.
       *
       *  object <moduleName> {
       *    def main(argv: Array[String]): Unit = {
       *      val args = argv
       *      new AnyRef {
       *        <stmts>
       *      }
       *    }
       *  }
       */
      import definitions.*

      def emptyPkg    = atPos(0, 0, 0) { Ident(nme.EMPTY_PACKAGE_NAME) }
      def emptyInit   = DefDef(
        Modifiers(),
        nme.CONSTRUCTOR,
        Nil,
        List(Nil),
        TypeTree(),
        Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), Nil)), Literal(Constant(())))
      )

      // def main
      def mainParamType = AppliedTypeTree(Ident(tpnme.Array), List(Ident(tpnme.String)))
      def mainParameter = List(ValDef(Modifiers(Param), "argv", mainParamType, EmptyTree))
      def mainSetArgv   = List(ValDef(Modifiers(), "args", TypeTree(), Ident("argv")))
      def mainNew       = makeNew(Nil, emptyValDef, stmts, List(Nil), NoPosition, NoPosition)
      def mainDef       = DefDef(Modifiers(), nme.main, Nil, List(mainParameter), scalaDot(tpnme.Unit), Block(mainSetArgv, mainNew))

      // object Main
      def moduleName  = ScriptRunner scriptMain settings
      def moduleBody  = Template(List(scalaScalaObjectConstr), emptyValDef, List(emptyInit, mainDef))
      def moduleDef   = ModuleDef(Modifiers(), moduleName, moduleBody)

      // package <empty> { ... }
      makePackaging(0, emptyPkg, List(moduleDef))
    }*/
