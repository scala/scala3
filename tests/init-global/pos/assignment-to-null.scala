class C {
  var f: Int = 1
}

object O {
  var c: C = null
  c = new C
  c.f = 2
}
