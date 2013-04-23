package dotty.tools.dotc
package core

import util.Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, TypedTrees._

object UntypedTrees {

  object untpd extends Trees.Instance[Untyped] {

  }

  import untpd._

  class UGen(implicit ctx: Context) {
    def constructor(mods: Modifiers, vparamAccessorss: List[List[Tree]], ofTrait: Boolean): DefDef = ???

    def Template(
      constrMods: Modifiers,
      vparamAccessorss: List[List[Tree]],
      parents: List[Tree],
      self: ValDef,
      stats: List[Tree],
      ofTrait: Boolean): Template = {
      val constr = constructor(constrMods, vparamAccessorss, ofTrait)
      Trees.Template(parents, self, vparamAccessorss.flatten ++ (constr :: stats))(NoPosition)
    }
  }

  def ugen(implicit ctx: Context) =
    new UGen
}

