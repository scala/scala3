object testindent:

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

  while true
  do println(1)

  for i <- List(1, 2, 3)
  do println(i)

  while (true)
  do println(1)

  for (i <- List(1, 2, 3))
  do println(i)
