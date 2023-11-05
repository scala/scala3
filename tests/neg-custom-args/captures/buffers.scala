import reflect.ClassTag

class Buffer[A]

class ArrayBuffer[sealed A: ClassTag] extends Buffer[A]:
  var elems: Array[A] = new Array[A](10)
  def add(x: A): this.type = ???
  def at(i: Int): A = ???

class ArrayBufferBAD[A: ClassTag] extends Buffer[A]:
  var elems: Array[A] = new Array[A](10) // error // error
  def add(x: A): this.type = ???
  def at(i: Int): A = ???

object ArrayBuffer:
  def make[A: ClassTag](xs: A*) = new ArrayBuffer: // error
    elems = xs.toArray
  def apply[sealed A: ClassTag](xs: A*) = new ArrayBuffer:
    elems = xs.toArray  // ok

class EncapsArray[A: ClassTag]:
  val x: Array[A] = new Array[A](10) // error








