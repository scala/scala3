// Tests that parts of the standard library the are copied from Scala 2 do work as expected with FieldNotFoundErrros

def assertThrows[T <: Throwable](thunk: => Unit)(using ct: reflect.ClassTag[T]): Unit =
  try
    thunk
    sys.error(s"Expected exception of type ${ct.runtimeClass.getName}, but no exception was thrown.")
  catch
    case ex: Throwable => assert(ct.runtimeClass.isInstance(ex), s"Expected exception of type ${ct.runtimeClass.getName}, but got ${ex.getClass.getName}.")

@main def Test(): Unit =
  locally:
    val it = Array.empty[Int].reverseIterator
    assert(!it.hasNext)
    assertThrows[NoSuchElementException]:
      it.next

  locally:
    val it = Range(1, 1).grouped(1)
    assert(!it.hasNext)
    assertThrows[NoSuchElementException]:
      it.next

  locally:
    val it = Range(0, 1).inits
    assert(it.hasNext)
    assert(it.next != null)
    assert(it.hasNext)
    assert(it.next != null)
    assert(!it.hasNext)
    assertThrows[NoSuchElementException]:
      it.next

  locally:
    val it = Range(0, 1).tails
    assert(it.hasNext)
    assert(it.next != null)
    assert(it.hasNext)
    assert(it.next != null)
    assert(!it.hasNext)
    assertThrows[NoSuchElementException]:
      it.next



