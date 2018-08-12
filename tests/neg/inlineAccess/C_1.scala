package p
private class D
class C {
  rewrite def inl(): Unit = {
    val d = new D()     // error (when inlined): not accessible
  }
}

