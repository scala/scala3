object testindent

  if (true)
    val x = 1
    println(x)

  while (false)
    val x = 1
    println(x)

  for (x <- List(1, 2, 3))
    val y = x
    println(y)

  for { x <- List(1, 2, 3) }
    val y = x
    println(y)

  for {
    x <- List(1, 2, 3)
  }
    val y = x
    println(y)

