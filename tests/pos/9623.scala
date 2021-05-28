object A {
  sealed trait TList
  sealed trait TNil extends TList
  sealed trait ++:[H, T <: TList] extends TList

  type :--[R <: TList, A] <: TList = R match {
    case (A ++: t) => t
    case (h ++: t) => h ++: (t :-- A)
  }
}

object B {
  import A.*

  type X = (Int ++: String ++: Double ++: TNil) :-- String

  class T[A]

  def f(ta: T[X]) = ()

  f(new T[Int ++: Double ++: TNil])
}
