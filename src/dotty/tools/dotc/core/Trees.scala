package dotty.tools.dotc.core

import Types._, Names._, Flags._, Positions._

object Trees {

  abstract class Modifiers {
    val flags: FlagSet
  }

  class MissingType {
    type Type
  }

  val missing: MissingType =
    new MissingType {
      type Type = Types.Type
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
         else shallowCopy).asInstanceOf[Tree[Type]]
      tree._tpe = tpe
      tree
    }

    def shallowCopy: Tree[T] = clone.asInstanceOf[Tree[T]]

  }

  case class Ident[T](name: Name)(implicit val pos: Position) extends Tree[T]

  case class Select[T](qualifier: Tree[T], name: Name)(implicit val pos: Position) extends Tree[T]

  case class Apply[T](fun: Tree[T], arg: Tree[T])(implicit val pos: Position) extends Tree[T]

  case class Pair[T](left: Tree[T], right: Tree[T])(implicit val pos: Position) extends Tree[T]

  case class ValDef[T](mods: Modifiers, name: Name, rtpe: Tree[T], rhs: Tree[T])(implicit val pos: Position) extends Tree[T]

  case class TypeDef[T](mods: Modifiers, name: Name, rhs: Tree[T])(implicit val pos: Position) extends Tree[T]

  case class DefDef[T](mods: Modifiers, name: Name, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], rtpe: Tree[T], rhs: Tree[T])(implicit val pos: Position) extends Tree[T]

  case class TypedSplice(tree: Tree[Type]) extends Tree[missing.Type] {
    def pos = tree.pos
  }

  implicit def embedTyped(tree: Tree[Type]): Tree[missing.Type] = TypedSplice(tree)

  class UnAssignedTypeException[T](tree: Tree[T]) extends Exception {
    override def getMessage: String = s"type of $tree is not assigned"
  }

}