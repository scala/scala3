trait Consumer1[T]:
  var x: Int = 1 // To force anonymous class generation
  def accept(x: T): Unit

trait Consumer2[T]:
  def accept(x: T): Unit

trait Producer1[T]:
  var x: Int = 1 // To force anonymous class generation
  def produce(x: Any): T

trait Producer2[T]:
  def produce(x: Any): T

trait ProdCons1[T]:
  var x: Int = 1 // To force anonymous class generation
  def apply(x: T): T

trait ProdCons2[T]:
  var x: Int = 1 // To force anonymous class generation
  def apply(x: T): T

object Test {
  def main(args: Array[String]): Unit = {
    val a1: Consumer1[? >: String] = x => ()
    a1.accept("foo")

    val a2: Consumer2[? >: String] = x => ()
    a2.accept("foo")

    val b1: Producer1[? <: String] = x => ""
    val bo1: String = b1.produce(1)

    val b2: Producer2[? <: String] = x => ""
    val bo2: String = b2.produce(1)

    val c1: ProdCons1[? <: String] = x => x
    val c2: ProdCons2[? <: String] = x => x
    // Can't do much with `c1` or `c2` but we should still pass Ycheck.
  }
}
