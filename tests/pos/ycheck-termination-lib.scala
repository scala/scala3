//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.{terminates}
  import scala.util.uncheckedTermination

  @terminates
  def reverse_:::[A, B >: A](tis: List[A], prefix: List[B]): List[B] = {
    var these: List[B] = tis
    var pres = prefix
    while (!pres.isEmpty) {
      these = pres.head :: these
      pres = pres.tail
    }
    these
  }

  @terminates
  def take[A](tis: List[A], n: Int): List[A] = if (tis.isEmpty || n <= 0) Nil else {
    val h = tis.head :: Nil
    var t = h
    var rest = tis.tail
    var i = 1
    while ({if (rest.isEmpty) return tis; i < n}) {
      i += 1
      val nx = rest.head :: Nil
      // t.next = nx
      t = nx
      rest = rest.tail
    }
    h
  }

  @terminates
  def map[A, B](tis: List[A], f: A => B): List[B] = {
    if (tis eq Nil) Nil else {
      val h = uncheckedTermination(f(tis.head)) :: Nil
      var t = h
      var rest = tis.tail
      while (rest ne Nil) {
        val nx = uncheckedTermination(f(rest.head)) :: Nil
        // t.next = nx
        t = nx
        rest = rest.tail
      }
      h
    }
  }

  @terminates
  def reverse[A](tis: List[A]): List[A] = {
    var result: List[A] = Nil
    var these = tis
    while (!these.isEmpty) {
      result = these.head :: result
      these = these.tail
    }
    result
  }

  @terminates
  def foldRight[A, B](tis: List[A])(z: B)(op: (A, B) => B): B = {
    var acc = z
    var these: List[A] = reverse(tis)
    while (!these.isEmpty) {
      acc = uncheckedTermination(op(these.head, acc))
      these = these.tail
    }
    acc
  }

}

