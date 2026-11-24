import caps.*

class IO extends SharedCapability
class File(val io: IO) { def read(): Int = 1 }
class Cell(val f: () => Int)

trait Fn[-A, +B] { def apply(x: A): B }
trait OnFile { def apply(x: File): Int }
trait OnFileCap { def apply(x: File^): Int }

// Closures of SAM types whose parameters have capability fields or capture sets
def test1: Fn[File, Int] = f => f.read()
def test2: OnFile = f => f.read()
def test3: OnFileCap = f => f.read()
def test4: Fn[Cell, Int] = c => c.f()
def test5: java.util.function.Function[File, Int] = f => f.read()
def test6: java.util.Comparator[File] = (a, b) => a.read() - b.read()
def test7(xs: java.util.List[File]): Unit = xs.forEach(f => f.read())

// Unboxing a parameter is charged to the closure
def test8(io: IO): Fn[File^{io}, Int]^{io} = f => f.read()
