package dotty.tools.dotc
package core

import Trees._, Positions._, Types._, Contexts._, Constants._, Names._
import SymDenotations._, Symbols._

object TypedTrees {

  class TypeTreeGen {
    implicit def pos(implicit ctx: Context): Position = ctx.position
    def Ident(tp: NamedType)(implicit ctx: Context): Ident[Type] =
      Trees.Ident(tp.name).withType(tp)
    def Select(pre: TypedTree, tp: NamedType)(implicit ctx: Context): Select[Type] =
      Trees.Select(pre, tp.name).withType(tp)
    def Apply(fn: TypedTree, args: List[TypedTree])(implicit ctx: Context): Apply[Type] = {
      val fntpe @ MethodType(pnames, ptypes) = fn.tpe
      assert(sameLength(ptypes, args))
      Trees.Apply(fn, args).withType(fntpe.instantiate(args map (_.tpe)))
    }
    def TypeTree(tp: Type)(implicit ctx: Context):TypeTree[Type] =
      Trees.TypeTree().withType(tp)
    def New(tp: Type)(implicit ctx: Context): New[Type] =
      Trees.New(TypeTree(tp))
    def Literal(const: Constant)(implicit ctx: Context): Literal[Type] =
      Trees.Literal(const).withType(const.tpe)
    def ArrayValue(elemtpt: TypedTree, elems: List[TypedTree])(implicit ctx: Context) =
      Trees.ArrayValue(elemtpt, elems).withType(defn.ArrayType.appliedTo(elemtpt.tpe))
    def NamedArg[T](name: Name, arg: TypedTree)(implicit ctx: Context) =
      Trees.NamedArg(name, arg).withType(arg.tpe)

    // ----------------------------------------------------------

    def New(tp: Type, args: List[TypedTree])(implicit ctx: Context): Apply[Type] =
      Apply(
        Select(
          New(tp),
          TermRef(tp.normalizedPrefix, tp.typeSymbol.primaryConstructor.asTerm)),
        args)
  }

  object tpd extends TypeTreeGen
}

