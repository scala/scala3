@main def Test =
  println(test1 {
    val a: Int = 1
    42: Int
  })
  println(test1 {
    val a: Int = 1
    a + a
  })

  println()

  println(test2 {
    val a: Int = 1
    val b: Int = 2
    42: Int
  })
  println(test2 {
    val a: Int = 1
    val b: Int = 2
    a + a
  })

  println(test2 {
    val a: Int = 1
    val b: Int = 2
    b + b
  })

  println(test2 {
    val a: Int = 1
    val b: Int = 2
    a + b
  })
