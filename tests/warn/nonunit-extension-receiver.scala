//> using options -Wnonunit-statement -Wvalue-discard

class Builder:
  def add(x: Int): this.type = this

extension (b: Builder) def addTop(x: Int): b.type = b

object Ops:
  extension (b: Builder) def addImported(x: Int): b.type = b
  extension [T](b: Builder) def addPoly(x: T): b.type = b
import Ops.*

given GivenOps: AnyRef with
  extension (b: Builder) def addGiven(x: Int): b.type = b

def mk: Builder = Builder()

def statements(b: Builder): Unit =
  var v = Builder()
  b.add(1)
  b.addGiven(1)
  b.addTop(1)
  b.addImported(1)
  b.addPoly("x")
  addTop(b)(1)
  v.add(1)
  v.addTop(1) // warn
  v.addGiven(1) // warn
  mk.add(1) // warn
  mk.addTop(1) // warn
  ()

def discards(b: Builder, c: Boolean): Unit =
  if c then b.add(1)
  else if !c then b.addGiven(1)
  else b.addTop(1)
