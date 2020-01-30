def k: Unit =
  val s: String = try
    "WUT"
  catch case _ => println("caught something") // error: Type Mismatch Error

  "OK" // warning: A pure expression does nothing in statement position

def block: String = try "" catch case _ => () // error: Type Mismatch Error
  "" // error: Expected a toplevel definition

def i: Int = try 0 catch
  case _ => () // error: Type Mismatch Error
  1 // error: Expected a toplevel definition
