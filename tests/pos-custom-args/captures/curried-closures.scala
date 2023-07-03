import java.io.*
import annotation.capability

def Test4(g: OutputStream^) =
  val xs: List[Int] = ???
  val later = (f: OutputStream^) => (y: Int) => xs.foreach(x => f.write(x + y))
  val _: (f: OutputStream^) ->{} Int ->{f} Unit = later

  val later2 = () => (y: Int) => xs.foreach(x => g.write(x + y))
  val _: () ->{} Int ->{g} Unit = later2
