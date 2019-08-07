package scala

package quoted {
  import scala.quoted.show.SyntaxHighlight

  sealed trait Type[T <: AnyKind] {
    type `$splice` = T

    /** Show a source code like representation of this type without syntax highlight */
    def show(implicit qctx: QuoteContext): String = qctx.show(this, SyntaxHighlight.plain)

    /** Show a source code like representation of this type */
    def show(syntaxHighlight: SyntaxHighlight)(implicit qctx: QuoteContext): String = qctx.show(this, syntaxHighlight)

  }

  /** Some basic type tags, currently incomplete */
  object Type {

    given UnitTag as Type[Unit] given (qctx: QuoteContext) = {
      import qctx.tasty._
      definitions.UnitType.seal.asInstanceOf[quoted.Type[Unit]]
    }

    given BooleanTag as Type[Boolean] given (qctx: QuoteContext) = {
      import qctx.tasty._
      definitions.BooleanType.seal.asInstanceOf[quoted.Type[Boolean]]
    }

    given ByteTag as Type[Byte] given (qctx: QuoteContext) = {
      import qctx.tasty._
      definitions.ByteType.seal.asInstanceOf[quoted.Type[Byte]]
    }

    given CharTag as Type[Char] given (qctx: QuoteContext) = {
      import qctx.tasty._
      definitions.CharType.seal.asInstanceOf[quoted.Type[Char]]
    }

    given ShortTag as Type[Short] given (qctx: QuoteContext) = {
      import qctx.tasty._
      definitions.ShortType.seal.asInstanceOf[quoted.Type[Short]]
    }

    given IntTag as Type[Int] given (qctx: QuoteContext) = {
      import qctx.tasty._
      definitions.IntType.seal.asInstanceOf[quoted.Type[Int]]
    }

    given LongTag as Type[Long] given (qctx: QuoteContext) = {
      import qctx.tasty._
      definitions.LongType.seal.asInstanceOf[quoted.Type[Long]]
    }

    given FloatTag as Type[Float] given (qctx: QuoteContext) = {
      import qctx.tasty._
      definitions.FloatType.seal.asInstanceOf[quoted.Type[Float]]
    }

    given DoubleTag as Type[Double] given (qctx: QuoteContext) = {
      import qctx.tasty._
      definitions.DoubleType.seal.asInstanceOf[quoted.Type[Double]]
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
