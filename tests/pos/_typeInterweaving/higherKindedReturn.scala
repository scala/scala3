def f_4[T]: [U] => T => T = ???
def f_3[T][U]: T => T = ???
def f_2[T][U](): T => T = ???
def f_1[T <: Int][U <: String](): T => T = ???
def f0[T <: Int][U <: String]: T => T = ???
def f1[T <: Int][U <: String]: [X <: Unit] => X => X = ???
def f2[T <: Int][U <: String](): [X <: Unit] => X => X = ???
def f3[T <: Int][U <: String]()[X <: Unit]: X => X = ???

@main def test = {
    f_4[Int][String] //only one that works when lines 1088 to 1089 of Applications.scala are uncommented
    f_3[Int][String]
    f_2[Int][String]()
    f_1[Int][String]()
    f0[Int][String]
    f1[Int][String]
    f1[Int][Unit]
    f1[Int][String][Unit]
    f2[Int]()[Unit]
    f3[Int]()[Unit]
}