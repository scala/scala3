object foo { def test = 23 }

def bar(x: Any) = x match { case m: foo.type => m.test } // need to call m.test