@main def test: Unit = {
  class C { type T1; type T2 }

  def pmatch(s: C): s.T2 = s match {
    case p: (C { type T1 = Int; type T2 >: T1  } & s.type) =>  // error
      (3: p.T1): p.T2
    case p: (C { type T1 = String; type T2 >: T1  } & s.type) =>  // error
      ("this branch should be matched": p.T1): p.T2
  }

  // ClassCastException: class java.lang.Integer cannot be cast to class java.lang.String
  val x = pmatch(new C { type T1 = String; type T2 = String })
}