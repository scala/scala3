import pkg.Facade

@main def Test(): Unit =
  println(Facade.getSentinel())
  println(Facade.SENTINEL)
  println(Facade.hidden())
  println(Facade.over(1))
  println(Facade.over(""))
  println(Facade.base())
  println(Facade.CONST)
  println(Facade.Nested.nested())
  println(pkg.Consts.iface())
  Facade.counter = 41
  Facade.counter += 1
  println(Facade.counter)
  locally:
    import Facade.*
    println(getSentinel())
    println(base())
  locally:
    import Facade.{base as b}
    println(b())
