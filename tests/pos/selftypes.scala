object selftypes {

  trait LinearSeqLike[+A, +Repr <: LinearSeqLike[A, Repr]] {
    self: Repr =>

    type X

    def toX: X

  }

  abstract class LS[+A] extends LinearSeqLike[A, LS[A]] {

  }

  val x: LS[Int] = null

  x.toX
}