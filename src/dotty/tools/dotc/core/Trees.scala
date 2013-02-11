package dotty.tools.dotc.core

import Types._, Names._, Flags._, Positions._, Contexts._, Constants._, SymDenotations._, Symbols._

object Trees {

  abstract class Modifiers {
    val flags: FlagSet
  }

  /** Trees take a parameter indicating what the type of their `tpe` field
   *  is. Two choices: `Types.Type` or `missing.Type`.
   *  Untyped trees have type `Tree[missing.Type]`. Because `missing.Type`
   *  is a completely abstract type, there's nothing one can do with it.
   *
   *  Tree typing uses a copy-on-write implementation:
   *
   *   - You can never observe a `tpe` which is `null` (throws an exception)
   *   - So when creating a typed tree with `withType` we can re-use
   *     the existing tree transparently, assigning its `tpe` field,
   *     provided it was `null` before.
   *   - It is impossible to embed untyped trees in typed ones.
   *   - It is possible to embed typed trees in untyped ones. In fact
   *     there is an implicit conversion from `Tree[Types.Type]` to
   *     `Tree[missing.Type]` which wraps the typed tree in a
   *     `TypedSplice` node.
   *   - Type checking an untyped tree will remove all embedded `TypedSplice`
   *     nodes.
   */
  abstract class Tree[T] {
    def pos: Position

    private var _tpe: T = _

    def tpe: T = {
      if (_tpe == null) throw new UnAssignedTypeException(this)
      _tpe
    }

    def withType(tpe: Type): Tree[Type] = {
      val tree =
        (if (_tpe == null ||
            (_tpe.asInstanceOf[AnyRef] eq tpe.asInstanceOf[AnyRef])) this
         else clone).asInstanceOf[Tree[Type]]
      tree._tpe = tpe
      tree
    }

    def withPosition(pos: Position) = ???
  }

  case class Ident[T] (name: Name)(implicit val pos: Position) extends Tree[T]

  case class Select[T](qualifier: Tree[T], name: Name)(implicit val pos: Position) extends Tree[T]

  case class Apply[T](fun: Tree[T], args: List[Tree[T]])(implicit val pos: Position) extends Tree[T]

  case class Pair[T](left: Tree[T], right: Tree[T])(implicit val pos: Position) extends Tree[T]

  case class ValDef[T](mods: Modifiers, name: Name, rtpe: Tree[T], rhs: Tree[T])(implicit val pos: Position) extends Tree[T]

  case class TypeDef[T](mods: Modifiers, name: Name, rhs: Tree[T])(implicit val pos: Position) extends Tree[T]

  case class DefDef[T](mods: Modifiers, name: Name, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], rtpe: Tree[T], rhs: Tree[T])(implicit val pos: Position) extends Tree[T]

  case class TypeTree[T](original: Tree[T] = EmptyTree[T])(implicit val pos: Position) extends Tree[T]

  case class EmptyTree[T]() extends Tree[T] {
    val pos = NoPosition
  }

  case class Literal[T](const: Constant)(implicit val pos: Position) extends Tree[T]

  case class New[T](tpt: Tree[T])(implicit val pos: Position) extends Tree[T]

  case class ArrayValue[T](elemtpt: Tree[T], elems: List[Tree[T]])(implicit val pos: Position) extends Tree[T]

  case class NamedArg[T](name: Name, arg: Tree[T])(implicit val pos: Position) extends Tree[T]

  class ImplicitDefDef[T](mods: Modifiers, name: Name, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], rtpe: Tree[T], rhs: Tree[T])
    (implicit pos: Position) extends DefDef[T](mods, name, tparams, vparamss, rtpe, rhs) {
    override def copy[T](mods: Modifiers, name: Name, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], rtpe: Tree[T], rhs: Tree[T])(implicit pos: Position) =
      new ImplicitDefDef[T](mods, name, tparams, vparamss, rtpe, rhs)
  }

  case class TypedSplice(tree: Tree[Type]) extends Tree[Nothing] {
    def pos = tree.pos
  }

  implicit def embedTyped(tree: Tree[Type]): Tree[Nothing] = TypedSplice(tree)

  class UnAssignedTypeException[T](tree: Tree[T]) extends Exception {
    override def getMessage: String = s"type of $tree is not assigned"
  }

  type TypedTree = Tree[Type]
  type UntypedTree = Tree[Nothing]

  // this will probably to its own file at some point.
  class MakeTypedTree(implicit val ctx: Context) extends AnyVal {
    implicit def pos = NoPosition
    def Ident(tp: NamedType) =
      Trees.Ident(tp.name).withType(tp)
    def Select(pre: TypedTree, tp: NamedType) =
      Trees.Select(pre, tp.name).withType(tp)
    def Apply(fn: TypedTree, args: List[TypedTree]) = {
      val fntpe @ MethodType(pnames, ptypes) = fn.tpe
      assert(sameLength(ptypes, args))
      Trees.Apply(fn, args).withType(fntpe.instantiate(args map (_.tpe)))
    }
    def TypeTree(tp: Type) =
      Trees.TypeTree().withType(tp)
    def New(tp: Type): TypedTree =
      Trees.New(TypeTree(tp))
    def Literal(const: Constant) =
      Trees.Literal(const).withType(const.tpe)
    def ArrayValue(elemtpt: TypedTree, elems: List[TypedTree]) =
      Trees.ArrayValue(elemtpt, elems).withType(defn.ArrayType.appliedTo(elemtpt.tpe))
    def NamedArg[T](name: Name, arg: TypedTree) =
      Trees.NamedArg(name, arg).withType(arg.tpe)

    // ----------------------------------------------------------

    def New(tp: Type, args: List[TypedTree]): TypedTree =
      Apply(
        Select(
          New(tp),
          TermRef(tp.normalizedPrefix, tp.typeSymbol.primaryConstructor.asTerm)),
        args)
  }

  def makeTypedTree(implicit ctx: Context) = new MakeTypedTree()(ctx)

}