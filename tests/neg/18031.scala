//> using options -source:future

object A:
  val x, y, z = 0


object B:
  export A.{*, x as _} // error

object C:
  import A.{*, x as _} // error


object D:
  export A.{x => blah} // error

object E:
  import A.{x => blah} // error


object F:
  export A._ // error

object G:
  import A._ // error
