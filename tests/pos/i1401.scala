package i1401

trait Subtractable[AS, +Repr <: Subtractable[AS, Repr]] {
  def -(elem: AS): Repr
}

trait BufferLike[BA, +Self <: BufferLike[BA, Self] with Buffer[BA]]
                extends Subtractable[BA, Self]
{ self : Self =>

  /* Without fix-#1401:
   *
     error: overriding method - in trait Subtractable of type (elem: A)Self & i1401.Buffer[A];
     method - of type (elem: BA)Self has incompatible type
     def -(elem: BA): Self
         ^
     one error found
  */
  def -(elem: BA): Self
}

trait Buffer[A] extends BufferLike[A, Buffer[A]]



