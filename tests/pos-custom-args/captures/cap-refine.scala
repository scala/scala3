//> using options -Werror
import caps.Capability

trait Buffer[T] extends Capability:
  def append(x: T): this.type

def f(buf: Buffer[Int]) =
  val buf1 = buf.append(1).append(2)
  val buf2: Buffer[Int]^{buf1} = buf1



