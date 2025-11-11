type Pos = {x: Int with x >= 0}

def safeDiv(x: Pos, y: Pos with y > 1): {res: Pos with res < x} =
  (x / y).runtimeChecked

object SafeSeqs:
  opaque type SafeSeq[T] = Seq[T]
  object SafeSeq:
    def fromSeq[T](seq: Seq[T]): SafeSeq[T] = seq
    def apply[T](elems: T*): SafeSeq[T] = fromSeq(elems)
  extension [T](a: SafeSeq[T])
    def len: Pos = a.length.runtimeChecked
    def apply(i: Pos with i < a.len): T =  a(i)
    def splitAt(i: Pos with i < a.len): (SafeSeq[T], SafeSeq[T]) = a.splitAt(i)
    def ++(that: SafeSeq[T]): SafeSeq[T] = a ++ that
  extension [T](a: SafeSeq[T] with a.len > 0)
    def head: T = a.head
    def tail: SafeSeq[T] = a.tail

import SafeSeqs.*

def merge[T: Ordering as ord](left: SafeSeq[T], right: SafeSeq[T]): SafeSeq[T] =
  (left, right) match
    case (left: SafeSeq[T] with left.len > 0, right: SafeSeq[T] with right.len > 0) =>
      if ord.lt(left.head, right.head) then SafeSeq(left.head) ++ merge(left.tail, right)
      else SafeSeq(right.head) ++ merge(left, right.tail)
    case _ =>
      if left.len == 0 then right
      else left

def mergeSort[T: Ordering](list: SafeSeq[T]): SafeSeq[T] =
  val len = list.len
  val middle = safeDiv(len, 2)
  if middle == 0 then
    list
  else
    val (left, right) = list.splitAt(middle)
    merge(mergeSort(left), mergeSort(right))

@main def Test =
  val nums = SafeSeq(5, 3, 8, 1, 2, 7, 4, 6)
  val sortedNums = mergeSort(nums)
  println(s"Unsorted: $nums")
  println(s"Sorted:   $sortedNums")

  val nums2 = SafeSeq(7, 4, 5, 3, 2, 6, 1)
  val sortedNums2 = mergeSort(nums2)
  println(s"Unsorted: $nums2")
  println(s"Sorted:   $sortedNums2")
