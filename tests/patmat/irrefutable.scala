sealed trait Base
sealed class A(s: String) extends Base
sealed class B(val n: Int) extends Base
sealed case class C(val s: String, val n: Int) extends Base

// boolean
object ExTrue {
  def unapply(b: Base): true = b match {
    case _ => true
  }

  def test(b: Base) = b match {
    case ExTrue() =>
  }
}

object ExFalse {
  def unapply(b: Base): Boolean = b match {
    case _ => true
  }

  def test(b: Base) = b match {  // warning
    case ExFalse() =>
  }
}

// product

object ExProduct {
  def unapply(b: B): (Int, Int) = (b.n, b.n * b.n)
  def test(b: B) = b match {
    case ExProduct(x, xx) =>
  }
}

// isEmpty/get

trait Res {
  def isEmpty: false = false
  def get: Int
}

object ExName {
  def unapply(b: B): Res = new Res { def get: Int = b.n }

  def test(b: B) = b match {
    case ExName(x) =>
  }
}

sealed class M(n: Int, s: String) extends Product {
  def _1: Int = n
  def _2: String = s
  def isEmpty: Boolean = s.size > n
  def get: M = this

  def canEqual(that: Any): Boolean = true
  def productArity: Int = 2
  def productElement(n: Int): Any = ???
}

object ExM {
  def unapply(m: M): M = m

  def test(m: M) = m match {  // warning
    case ExM(s) =>
  }
}

// some
object ExSome {
  def unapply(b: B): Some[Int] = Some(b.n)

  def test(b: B) = b match {
    case ExSome(x) =>
  }
}

object ExSome2 {
  def unapply(c: C): Some[C] = Some(c)

  def test(c: C) = c match {
    case ExSome2(s, n) =>
  }
}