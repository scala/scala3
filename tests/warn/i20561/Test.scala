def f(r: Rec): Unit = r match // warn
  case Rec(true) => ()

def g(r: Rec): Unit = r match
  case Rec(b) => ()

def h(r: Rec): Unit = r match
  case Rec(true) => ()
  case Rec(false) => ()

def i(x: Rec | String): Unit = x match // warn
  case Rec(_) => ()

def j(x: Rec | String): Unit = x match // warn
  case s: String => ()
