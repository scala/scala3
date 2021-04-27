object Test:

  type T[X] <: List[List[X]]

  var y =  // inferred type: Any, since `T[?]` is irreducible wildcard application
    val x: Any = null
    ??? : T[x.type]

  y = (??? : Any)
