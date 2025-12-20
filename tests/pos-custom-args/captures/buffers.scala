import reflect.ClassTag

class Buffer[A] extends caps.Mutable

class ArrayBuffer[A: ClassTag] extends Buffer[A]:
  var elems: Array[A] = new Array[A](10)
  def add(x: A): this.type = ???
  def at(i: Int): A = ???

class ArrayBufferBAD[A: ClassTag] extends Buffer[A]:
  var elems: Array[A] = new Array[A](10)
  def add(x: A): this.type = ???
  def at(i: Int): A = ???

object ArrayBuffer:
  def make[A: ClassTag](xs: A*) = new ArrayBuffer:
    elems = xs.toArray
  def apply[A: ClassTag](xs: A*) = new ArrayBuffer:
    elems = xs.toArray  // ok

class EncapsArray[A: ClassTag]:
  val x: Array[A] = new Array[A](10)








