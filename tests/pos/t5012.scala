class D {
  object p // (program point 1)
}

class C {
  def m: D | Null = {
    if ("abc".length == 0) {
      object p       // (program point 2)
    }
    null
  }
}
