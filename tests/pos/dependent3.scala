object BooleanListLengthFunction {
  dependent def length(l: LIST): Int =
    if (l.isInstanceOf[NIL.type]) 0
    else 1 + length(l.asInstanceOf[CONS].tail)

  sealed trait LIST
  dependent case object NIL extends LIST
  dependent case class CONS(head: Boolean, tail: LIST) extends LIST

  val a: 0 = length(NIL)
  val b: 1 = length(CONS(true, NIL))
  val c: 2 = length(CONS(true, CONS(false, NIL)))
  val d: 3 = length(CONS(true, CONS(false, CONS(true, NIL))))
}

object GenericListInstanceOf {
  sealed trait LIST[+T]
  dependent case object NIL extends LIST[Nothing]
  dependent case class CONS[+T](head: T, tail: LIST[T]) extends LIST[T]

  dependent def iioNIL(x: Any) = x.isInstanceOf[NIL.type]
  dependent def iioCONS(x: Any) = x.isInstanceOf[CONS[_]]

  val x1: true  = iioNIL(NIL)
  val x2: false = iioCONS(NIL)
  val x3: false = iioNIL(CONS(true, NIL))
  val x4: true  = iioCONS(CONS(true, NIL))

  dependent def iioNIL_T[T](x: LIST[T]) = x.isInstanceOf[NIL.type]
  dependent def iioCONS_T[T](x: LIST[T]) = x.isInstanceOf[CONS[_]]

  val x5: true  = iioNIL_T(NIL)
  val x6: false = iioCONS_T(NIL)
  val x7: false = iioNIL_T(CONS(true, NIL))
  val x8: true  = iioCONS_T(CONS(true, NIL))
}

object G {
  sealed trait LIST[+T]
  dependent case object NIL extends LIST[Nothing]
  dependent case class CONS[+T](head: T, tail: LIST[T]) extends LIST[T]

  dependent def AIO_tail(l: Any) = l.asInstanceOf[CONS[Boolean]].tail
  val nil: NIL.type = AIO_tail(CONS(true, NIL))
}

object GenericListLengthFunction {
  dependent def length[T](l: LIST[T]): Int =
    if (l.isInstanceOf[NIL.type]) 0
    else 1 + length(l.asInstanceOf[CONS[T]].tail)

  sealed trait LIST[+T]
  dependent case object NIL extends LIST[Nothing]
  dependent case class CONS[+T](head: T, tail: LIST[T]) extends LIST[T]

  val x1: 0 = length(NIL)
  val x2: 1 = length(CONS(true, NIL))
}

object GenericListLengthMethod {
  sealed trait LIST[+T] {
    dependent def length: Int =
      if (this.isInstanceOf[NIL.type]) 0
      else 1 + this.asInstanceOf[CONS[T]].tail.length
  }
  dependent case object NIL extends LIST[Nothing]
  dependent case class CONS[+T](head: T, tail: LIST[T]) extends LIST[T]

  val x1: 0 = NIL.length
  val x2: 1 = CONS(true, NIL).length
}
