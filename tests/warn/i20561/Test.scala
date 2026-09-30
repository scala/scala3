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

def k(r: Rec0): Unit = r match
  case Rec0() => ()

def l(x: Rec0 | String): Unit = x match // warn
  case Rec0() => ()

def m(r: RecVar): Unit = r match
  case RecVar(b, rest*) => ()

def n(r: RecVar): Unit = r match // warn
  case RecVar(true, rest*) => ()

def o(r: RecVar): Unit = r match // warn
  case RecVar(b, x, y) => ()

def p(r: RecVar): Unit = r match
  case RecVar(b, x, y) => ()
  case RecVar(b, rest*) => ()

def q(r: RecVarOnly): Unit = r match // warn
  case RecVarOnly(_, rest*) => ()
