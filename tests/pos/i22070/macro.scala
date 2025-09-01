trait Featureful[T]:
  def toFeatures(value: T): IArray[Float]

object Featureful:
  inline def derived[T](using scala.deriving.Mirror.Of[T]) = ${ derivedImpl[T] }

  import scala.quoted.*
  private def derivedImpl[T: Type](using Quotes): Expr[Featureful[T]] =
    import quotes.reflect.*
    '{
      new Featureful[T]:
        def toFeatures(value: T) =
          val feats   = IArray.empty[Featureful[?]]
          val product = value.asInstanceOf[Product]
          product.productIterator.zipWithIndex.foreach: (any, idx) =>
            feats(idx).toFeatures(any.asInstanceOf)
          IArray.empty
    }
