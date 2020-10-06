package scala.internal.quoted

import scala.quoted._
import scala.internal.tasty.CompilerInterface.quoteContextWithCompilerInterface

trait PickledType[T <: AnyKind]:
  def unpickle(): QuoteContext ?=> Type[T]

object PickledType:

  def make[T <: AnyKind](pickled: List[String], splices: PickledSplices): PickledType[T] =
    new PickledType[T]:
      def unpickle(): QuoteContext ?=> Type[T] =
        val bytes = TastyString.unpickle(pickled)
        val qctx = quoteContextWithCompilerInterface(summon[QuoteContext])
        val tree = qctx.reflect.unpickleTypeTree(bytes, splices)
        new scala.internal.quoted.Type(tree, qctx.hashCode).asInstanceOf[Type[T]]
