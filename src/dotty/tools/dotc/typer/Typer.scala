package dotty.tools
package dotc
package typer

import core._
import ast._
import Trees._, Constants._, StdNames._, Scopes._
import Contexts._, Symbols._, Types._, SymDenotations._, Names._, NameOps._, Flags._, Decorators._
import util.Positions._
import util.SourcePosition
import collection.mutable
import language.implicitConversions

trait TyperContextOps { ctx: Context => }


class Typer {
  def typed(tree: untpd.Tree, pt: Type)(implicit ctx: Context): tpd.Tree = ???
  def typedExpr(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): tpd.Tree = ???
  def typedType(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): tpd.Tree = ???
}