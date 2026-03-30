import caps.*
object Levels:

  trait Read extends Classifier, SharedCapability
  trait ReadWrite extends Classifier, SharedCapability

  class Box[T](acc: T):
    val access: T = acc

  def parReduce(xs: Seq[Int])(op: (Int, Int) ->{any.only[Read]} Int): Int = xs.reduce(op)

  @main def test =
    val r: Box[Read^] = ???
    val rw: Box[ReadWrite^] = ???
    val read: () ->{r.access*} Int = ???
    val write: Int ->{rw.access*} Unit = ???
    val checkRead: () ->{any.only[Read]} Int = read

    //read() // causes error with and without the println below
    parReduce(1 to 1000): (x, y) =>
      //println(r.access) // ok only if this is uncommented
      read() + read() // should be ok

    parReduce(1 to 1000): (x, y) =>
      x + y + read() // should be ok

    parReduce(1 to 1000): (x, y) => // error
      write(x)
      x + y + read()

