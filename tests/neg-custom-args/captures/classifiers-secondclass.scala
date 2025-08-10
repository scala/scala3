import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

// Test inspired by the "Gentrification Gone too Far?" paper
object Levels:

  trait Read extends Classifier, SharedCapability
  trait ReadWrite extends Classifier, SharedCapability

  trait File(val name: String):
    val r: Read^
    val rw: ReadWrite^
    // operations guarded by boxed capability members
    val read: () ->{r} Int
    val write: Int ->{rw} Unit

  object File:
    def apply(s: String): File^ = new File(s) {
      val r = new Read {}
      val rw = new ReadWrite {}
      val read = () =>
        println(s"Reading from $name with capability $r")
        42
      val write = (i: Int) =>
        println(s"Writing $i to $name with capability $rw")
    }

  // Unfortunately, we do not have @use lambdas yet
  trait UseFunction[U]:
    def apply[C^](f: File^{C}): U

  def withFile[U](name: String)(block: UseFunction[U]): U = block(File(name)) // unrestricted use of files & other capabilities
  def parReduce[U](xs: Seq[U])(op: (U, U) ->{cap.only[Read]} U): U = xs.reduce(op) // only Read-classified allowed

  @main def test =
    withFile("foo.txt"):
      new UseFunction[Unit]:
        def apply[C^](f: File^{C}): Unit =
          f.read() // ok
          parReduce(1 to 1000): (a, b) =>
            a * b * f.read()    // ok
          parReduce(1 to 1000): (a, b) => // error
            f.write(42)         // the error stems from here
            a + b + f.read()    // ok
          f.write(42)           // ok, unrestricted access to file

  def testMulti =
    withFile("foo.txt"):
      new UseFunction[Unit]:
        def apply[C^](f: File^{C}): Unit =
          withFile("bar.txt"):
            new UseFunction[Unit]:
              def apply[C^](g: File^{C}): Unit =
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

