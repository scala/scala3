package scala

package quoted {
  import scala.quoted.show.SyntaxHighlight

  sealed trait TypeTag[T <: AnyKind] {
    type `$splice` = T

    /** Show a source code like representation of this type without syntax highlight */
    def show(given qctx: QuoteContext): String = qctx.show(this, SyntaxHighlight.plain)

    /** Show a source code like representation of this type */
    def show(syntaxHighlight: SyntaxHighlight)(given qctx: QuoteContext): String = qctx.show(this, syntaxHighlight)

  }

  // TODO remove after reference compiler update
  @deprecated("Replaced by scala.quoted.TypeTag", "")
  sealed trait Type[T <: AnyKind] extends TypeTag[T]

  /** Some basic type tags, currently incomplete */
  object TypeTag {

    given UnitTag(given qctx: QuoteContext): TypeTag[Unit] = {
      import qctx.tasty.{_, given}
      defn.UnitType.seal.asInstanceOf[TypeTag[Unit]]
    }

    given BooleanTag(given qctx: QuoteContext): TypeTag[Boolean] = {
      import qctx.tasty.{_, given}
      defn.BooleanType.seal.asInstanceOf[TypeTag[Boolean]]
    }

    given ByteTag(given qctx: QuoteContext): TypeTag[Byte] = {
      import qctx.tasty.{_, given}
      defn.ByteType.seal.asInstanceOf[TypeTag[Byte]]
    }

    given CharTag(given qctx: QuoteContext): TypeTag[Char] = {
      import qctx.tasty.{_, given}
      defn.CharType.seal.asInstanceOf[TypeTag[Char]]
    }

    given ShortTag(given qctx: QuoteContext): TypeTag[Short] = {
      import qctx.tasty.{_, given}
      defn.ShortType.seal.asInstanceOf[TypeTag[Short]]
    }

    given IntTag(given qctx: QuoteContext): TypeTag[Int] = {
      import qctx.tasty.{_, given}
      defn.IntType.seal.asInstanceOf[TypeTag[Int]]
    }

    given LongTag(given qctx: QuoteContext): TypeTag[Long] = {
      import qctx.tasty.{_, given}
      defn.LongType.seal.asInstanceOf[TypeTag[Long]]
    }

    given FloatTag(given qctx: QuoteContext): TypeTag[Float] = {
      import qctx.tasty.{_, given}
      defn.FloatType.seal.asInstanceOf[TypeTag[Float]]
    }

    given DoubleTag(given qctx: QuoteContext): TypeTag[Double] = {
      import qctx.tasty.{_, given}
      defn.DoubleType.seal.asInstanceOf[TypeTag[Double]]
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
