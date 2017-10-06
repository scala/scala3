object Test {

  def f: Boolean = true

  if (true)
    println("hi")

  if (false)
    println("hi")

  if (true && f)
    println("hi")

  if (false && f)
    println("hi")

  if (true || f)
    println("hi")

  if (false || f)
    println("hi")

}
