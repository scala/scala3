
object typeApply:

    def f0[T]: [U] => T => T = ???
    def f1[T](using DummyImplicit)[U]: T => T = ???
    def f2[T](using DummyImplicit)[U](): T => T = ???
    def f3[T <: Int](using DummyImplicit)[U <: String](): T => T = ???
    def f4[T <: Int](using DummyImplicit)[U <: String]: T => T = ???
    def f5[T <: Int](using DummyImplicit)[U <: String]: [X <: Unit] => X => X = ???
    def f6[T <: Int](using DummyImplicit)[U <: String](): [X <: Unit] => X => X = ???
    def f7[T <: Int](using DummyImplicit)[U <: String]()[X <: Unit]: X => X = ???

    @main def test = {
        f0[Int][String]
        f1[Int][String]
        f2[Int][String]()
        f3[Int][String]()
        f4[Int][String]
        f5[Int][String]
        f5[Int][String][Unit]
        f6[Int]()[Unit]
        f7[Int]()[Unit]
    }
