package scala

package quoted {
  import scala.quoted.show.SyntaxHighlight

  sealed trait Type[T <: AnyKind] {
    type `$splice` = T

    /** Show a source code like representation of this type without syntax highlight */
    def show(given qctx: QuoteContext): String = qctx.show(this, SyntaxHighlight.plain)

    /** Show a source code like representation of this type */
    def show(syntaxHighlight: SyntaxHighlight)(given qctx: QuoteContext): String = qctx.show(this, syntaxHighlight)

  }

  /** Some basic type tags, currently incomplete */
  object Type {

    given UnitTag(given qctx: QuoteContext): Type[Unit] = {
      import qctx.tasty._
      defn.UnitType.seal.asInstanceOf[quoted.Type[Unit]]
    }

    given BooleanTag(given qctx: QuoteContext): Type[Boolean] = {
      import qctx.tasty._
      defn.BooleanType.seal.asInstanceOf[quoted.Type[Boolean]]
    }

    given ByteTag(given qctx: QuoteContext): Type[Byte] = {
      import qctx.tasty._
      defn.ByteType.seal.asInstanceOf[quoted.Type[Byte]]
    }

    given CharTag(given qctx: QuoteContext): Type[Char] = {
      import qctx.tasty._
      defn.CharType.seal.asInstanceOf[quoted.Type[Char]]
    }

    given ShortTag(given qctx: QuoteContext): Type[Short] = {
      import qctx.tasty._
      defn.ShortType.seal.asInstanceOf[quoted.Type[Short]]
    }

    given IntTag(given qctx: QuoteContext): Type[Int] = {
      import qctx.tasty._
      defn.IntType.seal.asInstanceOf[quoted.Type[Int]]
    }

    given LongTag(given qctx: QuoteContext): Type[Long] = {
      import qctx.tasty._
      defn.LongType.seal.asInstanceOf[quoted.Type[Long]]
    }

    given FloatTag(given qctx: QuoteContext): Type[Float] = {
      import qctx.tasty._
      defn.FloatType.seal.asInstanceOf[quoted.Type[Float]]
    }

    given DoubleTag(given qctx: QuoteContext): Type[Double] = {
      import qctx.tasty._
      defn.DoubleType.seal.asInstanceOf[quoted.Type[Double]]
    }

  }

}

package internal {
  package quoted {

    /** An Type backed by a tree */
    final class TreeType[Tree](val typeTree: Tree, val scopeId: Int) extends scala.quoted.Type[Any] {
      override def toString: String = s"Type(<tasty tree>)"
    }

  }
}
