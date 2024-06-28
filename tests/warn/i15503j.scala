//> using options  -Wunused:strict-no-implicit-warn

package foo.unused.strict.test:
  package a:
    given x: Int = 0
    implicit val y: Int = 1
    val z: Int = 2
    def f: Int = 3
  package b:
    import a.given // OK
    import a._ // OK
    import a.* // OK
    import a.x // OK
    import a.y // OK
    import a.z // warn
    import a.f // warn
  package c:
    import a.given // OK
    import a.x // OK
    import a.y // OK
    import a.z // OK
    import a.f // OK
    def g = f + z + y + x

package foo.implicits.resolution:
  class X
  class Y extends X
  object A { implicit val x: X = new X }
  object B { implicit val y: Y = new Y }
  class C {
    import A._ // OK
    import B._ // OK
    def t = implicitly[X]
  }

package foo.unused.summon.inlines:
  package lib:
    trait A
    trait B
    trait C
    trait X

    given willBeUnused: (A & X) = new A with X {}
    given willBeUsed: (A & B) = new A with B {}

  package use:
    import lib.{A, B, C, willBeUnused, willBeUsed} //OK
    import compiletime.summonInline //OK

    transparent inline given conflictInside: C =
      summonInline[A]
      new {}

    transparent inline given potentialConflict: C =
      summonInline[B]
      new {}

    val b: B = summon[B]
    val c: C = summon[C]