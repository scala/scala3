
object typeApply:

    def f3[T <: Int](using DummyImplicit)[U <: String](): T => T = ???
    def f5[T <: Int](using DummyImplicit)[U <: String]: [X <: Unit] => X => X = ???
    def f7[T <: Int](using DummyImplicit)[U <: String]()[X <: Unit]: X => X = ???

    @main def test = {
        f3[String]() // error
        f5[Int][Unit] // error
        f5[String][Unit] // error // error
        f7[String]()[Unit] // error
    }
