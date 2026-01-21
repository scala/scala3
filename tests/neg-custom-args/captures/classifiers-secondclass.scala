import language.experimental.captureChecking
import language.experimental.separationChecking
import language.experimental.erasedDefinitions

import caps.*

// Test inspired by the "Gentrification Gone too Far?" paper
object Levels:

  /* User-defined capability classifiers */
  trait Read extends Classifier, SharedCapability
  trait ReadWrite extends Classifier, SharedCapability

  trait File(val name: String):
    // file-specific capability members, these are ensured to be erased
    erased val r: Read^
    erased val rw: ReadWrite^
    // operations guarded by capability members
    val read: () ->{r} Int
    val write: Int ->{rw} Unit

  object File:
    def apply(s: String): File^ = new File(s) {
      erased val r = new Read {}
      erased val rw = new ReadWrite {}
      val read = () =>
        println(s"Reading from $name with capability $r")
        42
      val write = (i: Int) =>
        println(s"Writing $i to $name with capability $rw")
    }

  def withFile[U](name: String)(block: File^ => U): U = block(File(name)) // unrestricted use of files & other capabilities
  def parReduce[U](xs: Seq[U])(op: (U, U) ->{any.only[Read]} U): U = xs.reduce(op) // only Read-classified allowed

  @main def test =
    withFile("foo.txt"): f =>
      f.read() // ok
      parReduce(1 to 1000): (a, b) =>
        a * b * f.read()    // ok
      parReduce(1 to 1000): (a, b) => // error
        f.write(42)         // the error stems from here
        a + b + f.read()    // ok
      f.write(42)           // ok, unrestricted access to file

  def testMulti =
    withFile("foo.txt"): f =>
      withFile("bar.txt"): g =>
        f.read() // ok
        g.read() // ok
        parReduce(1 to 1000): (a, b) =>
          a * b * f.read() + g.read() // ok
        parReduce(1 to 1000): (a, b) => // error
          f.write(42)         // the error stems from here
          a + b + f.read() + g.read() // ok
        parReduce(1 to 1000): (a, b) => // error
          g.write(42)         // the error stems from here
          0
        f.write(42)           // ok, unrestricted access to file
        g.write(42)           // ok, unrestricted access to file

