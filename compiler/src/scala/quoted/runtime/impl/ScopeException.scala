package scala.quoted.runtime.impl

import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.core.Contexts._

class ScopeException(msg: String) extends Exception(msg)

object ScopeException:
  def checkInCorrectScope(scope: Scope, currentScope: Scope, tree: Tree, kind: String)(using Context): Unit =
    if scope.root != currentScope.root then
      throw new ScopeException(s"Cannot use $kind oustide of the macro splice `$${...}` or the scala.quoted.staging.run(...)` where it was defined")

    if ctx.settings.XcheckMacros.value && !scope.isOuterScopeOf(currentScope) then
      throw new ScopeException(
        if scope.atSameLocation(currentScope) then
          s"""Type created in a splice, extruded from that splice and then used in a subsequent evaluation of that same splice.
          |Splice: $scope
          |$kind: ${tree.show}
          |
          |
          |Splice stack:
          |${scope.stack.mkString("\t", "\n\t", "\n")}
          """.stripMargin
        else
          s"""Expression created in a splice was used outside of that splice.
            |Created in: $scope
            |Used in: $currentScope
            |$kind: ${tree.show}
            |
            |
            |Creation stack:
            |${scope.stack.mkString("\t", "\n\t", "\n")}
            |
            |Use stack:
            |${currentScope.stack.mkString("\t", "\n\t", "\n")}
            |
            |Hint: A common reason for this to happen is when a `def` that creates a `'{...}`
            |      captures an outer instance of `Quotes`. If this `def` is called in a splice
            |      it will not track the `Quotes` provided by that particular splice.
            |      To fix it add a `given Quotes` to this `def`.
          """.stripMargin)
