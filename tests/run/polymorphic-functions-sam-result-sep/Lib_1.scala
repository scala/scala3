trait MySam[A]:
  def apply(x: A): A

trait GenSam[T]:
  def apply(x: T): T
trait IntSam extends GenSam[Int]:
  def apply(x: Int): Int

trait Helper:
  def twice(x: Int): Int = 2 * x
trait WithHelper[A] extends Helper:
  def apply(x: A): Int
