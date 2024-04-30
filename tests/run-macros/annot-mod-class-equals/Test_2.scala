//> using options -experimental

@equals class Foo(val a: String, val b: Int)
  //> override def equals(that: Any): Boolean =
  //>   that match
  //>     case that: Foo => this.a.==(that.a).&&(this.b.==(that.b))
  //>     case _ => false
  //> private lazy val hash$macro$1: Int =
  //>   var acc = -1566937476 // scala.runtime.Statics.mix(-889275714, "Foo".hashCode)
  //>   acc = scala.runtime.Statics.mix(acc, scala.runtime.Statics.anyHash(a))
  //>   acc = scala.runtime.Statics.mix(acc, b)
  //>   scala.runtime.Statics.finalizeHash(acc, 2)
  //> override def hashCode(): Int = hash$macro$1

@equals class Bar(val a: String)
  //> override def equals(that: Any): Boolean =
  //>   that match
  //>     case that: Bar => this.a.==(that.a)
  //>     case _ => false
  //> private lazy val hash$macro$2: Int =
  //>   var acc = 1555242735 // scala.runtime.Statics.mix(-889275714, "Bar".hashCode)
  //>   acc = scala.runtime.Statics.mix(acc, scala.runtime.Statics.anyHash(a))
  //>   scala.runtime.Statics.finalizeHash(acc, 1)
  //> override def hashCode(): Int = hash$macro$2

@main def Test(): Unit =
  assert(Foo("abc", 2) == Foo("abc", 2))
  assert(Foo("abc", 2).hashCode() == Foo("abc", 2).hashCode())

  assert(Bar("abc") == Bar("abc"))
  assert(Bar("abc").hashCode() == Bar("abc").hashCode())
