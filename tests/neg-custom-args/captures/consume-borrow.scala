import language.experimental.separationChecking
import caps.*

class Ref extends Mutable:
  consume def forget(): Ref^ = this

def badQualifier(a: Ref^): Ref^ =
  a.forget() // error

def badAliasedQualifier(a: Ref^): Ref^ =
  val b = a
  b.forget() // error

def badResult(a: Ref^): Ref^ = a // error

def goodQualifier(consume a: Ref^): Ref^ =
  a.forget()

def goodResult(a: Ref^): a.type = a
