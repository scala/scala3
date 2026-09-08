package lib

import scala.quoted.*

object Wrapper:
  opaque type Impl = Int
  object Impl:
    def apply(x: Int): Impl = x

type MyType = Wrapper.Impl
object MyType:
  def apply(x: Int): MyType = Wrapper.Impl(x)

object MyMacro:
  def createImpl(using Quotes): Expr[MyType] =
    '{ MyType(42) }

  inline def create: MyType = ${ createImpl }
