object o:
  trait T private[o]()

def test = new o.T { } // error
