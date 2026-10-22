import J.*

def varargOnly(r: RecVarOnly): Unit =
  r match
    case RecVarOnly(xs*) => xs.foreach(x => ())
  r match
    case RecVarOnly(a, rest*) => ()
    case _ => ()

def varargWithPrefix(r: RecVar): Unit =
  r match
    case RecVar(x, xs*) => xs.foreach(x => ())
  r match
    case RecVar(x, a, b) => ()
    case _ => ()
