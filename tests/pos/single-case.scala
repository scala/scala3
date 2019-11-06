object test with

  type T[X] = X match case Int => X

  try
    println("hi")
  catch case ex: java.io.IOException => println("ho")

  1 match case x: Int => println(x)


