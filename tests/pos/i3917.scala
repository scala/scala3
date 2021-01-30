class A {
  var a = false
}

object B {
  var b = false
}

class C {
  var c = false
}

object C extends A {
  def test = {
    a = true
    C.a = true
    this.a = true
    C.this.a = true

    import B.*
    b = true

    val c0 = new C
    import c0.*
    c = true
  }
}
