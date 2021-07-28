package tests.emptyparens

class C {
  def f1()(implicit i: Int) = i

  def f2()(using i: Int) = i

  def f3(s: String)(implicit i: Int) = i

  def f4(s: String)(using i: Int) = i

  def f5()()(using i: Int) = i

  def f6() = 1

  def f7()() = 2

  def f8(i: Int)() = 1
}

class C1()(implicit i: Int)

class C2()(using i: Int)

class C3()()

class C4()(i: Int)