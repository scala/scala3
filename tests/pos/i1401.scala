package i1401

trait Subtractable[AS, +Repr <: Subtractable[AS, Repr]] {
  def -(elem: AS): Repr
}

trait BufferLike[BA, +This <: BufferLike[BA, This] with Buffer[BA]]
                extends Subtractable[BA, This]
{ self : This =>

  /* Without fix-#1401:
   *
     error: overriding method - in trait Subtractable of type (elem: A)This & i1401.Buffer[A];
     method - of type (elem: BA)This has incompatible type
     def -(elem: BA): This
         ^
     one error found
  */
  def -(elem: BA): This
}

trait Buffer[A] extends BufferLike[A, Buffer[A]]



