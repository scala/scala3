abstract class B

class C(var o: () => B | Int) extends B

class D(var o: () => B | Int) extends B

object A:
  def f1(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f2(c, i + 1)
    c = f1(c, i + 1)
    c

  def f2(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f3(c, i + 1)
    c

  def f3(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f4(c, i + 1)
    c

  def f4(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f5(c, i + 1)
    c

  def f5(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f6(c, i + 1)
    c

  def f6(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f7(c, i + 1)
    c

  def f7(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f8(c, i + 1)
    c

  def f8(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f9(c, i + 1)
    c

  def f9(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f10(c, i + 1)
    c

  def f10(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f11(c, i + 1)
    c

  def f11(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f12(c, i + 1)
    c

  def f12(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f13(c, i + 1)
    c

  def f13(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f14(c, i + 1) // non-termination if use f1
    c

  def f14(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f15(c, i + 1)
    c

  def f15(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f16(c, i + 1)
    c

  def f16(a: () => B, i: Int): () => B =
    var c = () => if i % 2 == 0 then new C(a) else new D(a)
    c = f1(c, i + 1)
    c

  val c = f1(() => new C(() => 3), 10)
