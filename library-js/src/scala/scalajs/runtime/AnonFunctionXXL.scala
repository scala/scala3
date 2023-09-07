package scala.scalajs.runtime

import scala.scalajs.js

@inline
final class AnonFunctionXXL(f: js.Function1[IArray[Object], Object]) extends scala.runtime.FunctionXXL {
  override def apply(xs: IArray[Object]): Object = f(xs)
}
