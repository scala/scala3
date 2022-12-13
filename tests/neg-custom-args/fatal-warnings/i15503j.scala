// scalac: -Wunused:strict-no-implicit-warn

package unused.test:
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
    import a.z // error
    import a.f // error
  package c:
    import a.given // OK
    import a._ // OK
    import a.* // OK
    import a.x // OK
    import a.y // OK
    import a.z // OK
    import a.f // OK
    def g = f + z + y + x