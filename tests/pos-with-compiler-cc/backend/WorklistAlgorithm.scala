package dotty.tools
package backend

/**
 * Simple implementation of a worklist algorithm. A processing
 * function is applied repeatedly to the first element in the
 * worklist, as long as the stack is not empty.
 *
 * The client class should mix-in this class and initialize the worklist
 * field and define the `processElement` method. Then call the `run` method
 * providing a function that initializes the worklist.
 *
 * @author  Martin Odersky
 * @version 1.0
 * @see     [[scala.tools.nsc.backend.icode.Linearizers]]
 */
trait WorklistAlgorithm {
  type Elem
  class WList {
    private var list: List[Elem] = Nil
    def isEmpty = list.isEmpty
    def nonEmpty = !isEmpty
    def push(e: Elem): Unit = { list = e :: list }
    def pop(): Elem = {
      val head = list.head
      list = list.tail
      head
    }
    def pushAll(xs: Iterable[Elem]): Unit = xs.foreach(push)
    def clear(): Unit = list = Nil

  }

  val worklist: WList

  /**
   * Run the iterative algorithm until the worklist remains empty.
   * The initializer is run once before the loop starts and should
   * initialize the worklist.
   */
  def run(initWorklist: => Unit) = {
    initWorklist

    while (worklist.nonEmpty)
      processElement(dequeue)
  }

  /**
   * Process the current element from the worklist.
   */
  def processElement(e: Elem): Unit

  /**
   * Remove and return the first element to be processed from the worklist.
   */
  def dequeue: Elem
}
