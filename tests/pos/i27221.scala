//> using options -language:experimental.inlineTraits
import language.experimental.captureChecking

trait Iterator3 extends IterableOnce3 with IterableOnceOps3

trait IterableOnce3 { this: IterableOnce3^ =>
  def iterator: Iterator3^{this}
}

inline trait IterableOnceOps3:
  def copyToArray(i: Int): Unit = ???
  def copyToArray(i: Int, j: Int): Unit = ???
  def copyToArray(i: Int, j: Int, k: Int): Unit = ???

object IterableOnce3:
  def copyElemsToArray(elems: IterableOnce3^, i: Int): Unit = 
    elems.iterator.copyToArray(i)
