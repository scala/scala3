package scala.collection.mutable

import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*
import org.scalacheck.*

object MutablePriorityQueueTest extends Properties("PriorityQueue") {
  type E = Int // the element type used for most/all tests

  def checkInvariant[A](pq: PriorityQueue[A])(implicit ord: Ordering[A]): Boolean = {
    // The ordering invariant in the heap is that parent >= child.
    // A child at index i has a parent at index i/2 in the priority
    // queue's internal array.  However, that array is padded with
    // an extra slot in front so that the first real element is at
    // index 1.  The vector below is not padded, so subtract 1 from
    // every index.
    import ord._
    val vec = pq.toVector // elements in same order as pq's internal array
    2 until pq.size forall { i => vec(i/2-1) >= vec(i-1) }
  }

  property("newBuilder (in companion)") = forAll { (list: List[E]) =>
    val builder = PriorityQueue.newBuilder[E]
    for (x <- list) builder += x
    val pq = builder.result()
    checkInvariant(pq) &&
    pq.dequeueAll == list.sorted.reverse
  }

  property("to[PriorityQueue]") = forAll { (list: List[E]) =>
    val pq = list.to(PriorityQueue)
    checkInvariant(pq) &&
    pq.dequeueAll == list.sorted.reverse
  }

  property("apply (in companion)") = forAll { (list: List[E]) =>
    val pq = PriorityQueue.apply(list*)
    checkInvariant(pq) &&
    pq.dequeueAll == list.sorted.reverse
  }

  property("size, isEmpty") = forAll { (list: List[E]) =>
    val pq = PriorityQueue(list*)
    pq.size == list.size && pq.isEmpty == list.isEmpty
  }

  property("+=") = forAll { (x: E, list: List[E]) =>
    val pq = PriorityQueue(list*)
    pq += x
    checkInvariant(pq) &&
    pq.dequeueAll == (x :: list).sorted.reverse
  }

  property("++= on empty") = forAll { (list: List[E]) =>
    val pq = PriorityQueue.empty[E]
    pq ++= list
    checkInvariant(pq) &&
    pq.dequeueAll == list.sorted.reverse
  }

  property("++=") = forAll { (list1: List[E], list2: List[E]) =>
    val pq = PriorityQueue(list1*)
    pq ++= list2
    checkInvariant(pq) &&
    pq.dequeueAll == (list1 ++ list2).sorted.reverse
  }

  property("reverse") = forAll { (list: List[E]) =>
    val pq = PriorityQueue(list*).reverse
    checkInvariant(pq)(using implicitly[Ordering[E]].reverse) &&
    pq.dequeueAll == list.sorted
  }

  property("reverse then ++=") = forAll { (list: List[E]) =>
    val pq = PriorityQueue.empty[E].reverse ++= list
    checkInvariant(pq)(using implicitly[Ordering[E]].reverse) &&
    pq.dequeueAll == list.sorted
  }

  property("reverse then +=") = forAll { (x: E, list: List[E]) =>
    val pq = PriorityQueue(list*).reverse += x
    checkInvariant(pq)(using implicitly[Ordering[E]].reverse) &&
    pq.dequeueAll == (x +: list).sorted
  }

  property("clone") = forAll { (list: List[E]) =>
    val pq = PriorityQueue(list*)
    val c = pq.clone()
    (pq ne c) &&
    checkInvariant(c) &&
    c.dequeueAll == pq.dequeueAll
  }

  property("dequeue") = forAll { (list: List[E]) =>
    list.nonEmpty ==> {
      val pq = PriorityQueue(list*)
      val x = pq.dequeue()
      checkInvariant(pq) &&
      x == list.max && pq.dequeueAll == list.sorted.reverse.tail
    }
  }

}
