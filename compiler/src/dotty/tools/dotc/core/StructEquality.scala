package dotty.tools.dotc
package core

import annotation.tailrec
import Types._

class StructEquality(binders1: Array[BindingType], binders2: Array[BindingType]) {

  def equals(tp1: Type, tp2: Any): Boolean = (tp1 `eq` tp2.asInstanceOf[AnyRef]) || tp1.iso(tp2, this)

  @tailrec final def equals(tps1: List[Type], tps2: List[Type]): Boolean =
    (tps1 `eq` tps2) || {
      if (tps1.isEmpty) tps2.isEmpty
      else tps2.nonEmpty && equals(tps1.head, tps2.head) && equals(tps1.tail, tps2.tail)
    }

  final def equalBinders(tp1: BindingType, tp2: BindingType): Boolean =
    (tp1 `eq` tp2) || {
      var idx = 0
      while (idx < binders1.length && (tp1 `ne` binders1(idx)))
        idx += 1
      idx < binders2.length && (tp2 `eq` binders2(idx))
    }

  final def withBinders(binder1: BindingType, binder2: BindingType): StructEquality = {
    val newBinders1 = new Array[BindingType](binders1.length + 1)
    val newBinders2 = new Array[BindingType](binders2.length + 1)
    Array.copy(binders1, 0, newBinders1, 0, binders1.length)
    Array.copy(binders2, 0, newBinders2, 0, binders2.length)
    newBinders1(binders1.length) = binder1
    newBinders2(binders1.length) = binder2
    new StructEquality(newBinders1, newBinders2)
  }
}

object StructEquality extends StructEquality(Array(), Array())