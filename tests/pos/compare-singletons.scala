class Refl {
  type S
}

class A[R <: Refl & Singleton](val r: R) {
  def s: r.S = ???
}

class B[R <: Refl & Singleton](val r: R) {
  val a = new A(r)
  val s: r.S = a.s
}