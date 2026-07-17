type Pos = {x: Int with x >= 0}

def safeDiv(x: Pos, y: Pos with y > 1): {res: Pos with res < x} =
  (x / y).runtimeChecked

trait SafeSeq[T]:
  def len: Pos
  def apply(i: Pos with i < this.len): T
  def splitAt(i: Pos with i < this.len): {p: (SafeSeq[T], SafeSeq[T]) with p._1.len + p._2.len == this.len}
  def ++(that: SafeSeq[T]): {res: SafeSeq[T] with res.len == this.len + that.len}
  def head: T
  def take(n: Pos): {res: SafeSeq[T] with res.len == n}
  def tail: {res: SafeSeq[T] with res.len == this.len - 1}

def merge[T: Ordering as ord](left: SafeSeq[T], right: SafeSeq[T]): {res: SafeSeq[T] with res.len == left.len + right.len} =
  if left.len > 0 && right.len > 0 then
    if ord.lt(left.head, right.head) then left.take(1) ++ merge(left.tail, right)
    else right.take(1) ++ merge(left, right.tail)
  else if left.len == 0 then right
  else left

def mergeSort[T: Ordering](list: SafeSeq[T]): {res: SafeSeq[T] with res.len == list.len} =
  val len = list.len
  val middle = safeDiv(len, 2)
  if middle == 0 then list
  else
    val (left, right) = list.splitAt(middle)
    merge(mergeSort(left), mergeSort(right))

def test(s: SafeSeq[Int] with s.len == 8) =
  mergeSort(s): {s: SafeSeq[Int] with s.len == 8}
  ()
