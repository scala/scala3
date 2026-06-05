package scala.collection.immutable

import org.junit.Test

import tools.AllocationTest

class SeqTest extends AllocationTest {

  @Test def emptyNonAllocating(): Unit = {
    nonAllocating(Seq.empty)
    nonAllocating(Seq())
  }

}
