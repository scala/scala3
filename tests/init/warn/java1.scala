import java.util.Spliterator
import java.util.function.Consumer

class A extends Spliterator.OfDouble:
  def characteristics() = 10
  def estimateSize() = 10
  def trySplit() = ???
  def tryAdvance(x$0: java.util.function.DoubleConsumer): Boolean = false

  val m = n + 1
  val n = 10     // warn
