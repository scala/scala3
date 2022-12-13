// scalajs: --skip
object Test:
  def main(args: Array[String]): Unit =
    go(classOf[MyTable])
    go(classOf[MyTable2])

  def go(cls: Class[?]): Unit =
    for c <- cls.getDeclaredConstructors.sortBy(_.getName) do
      c.setAccessible(true)
      println(s"inspecting constructor ${c.getName}")
      for p <- c.getParameters.sortBy(_.getName) do
        print(s"inspecting param ${p.getName}")
        for a <- p.getAnnotations.sortBy(_.annotationType.toString) do
          print(s" @${a.annotationType.getName}")
        println()

    for (m <- cls.getDeclaredFields.sortBy(_.getName)) {
      m.setAccessible(true)
      print(s"inspecting field ${m.getName}")
      for a <- m.getAnnotations().sortBy(_.annotationType.toString) do
        print(s" @${a.annotationType.getName}")
      println()
    }

    for (m <- cls.getDeclaredMethods.sortBy(_.getName)) {
      m.setAccessible(true)
      print(s"inspecting method ${m.getName}")
      for a <- m.getAnnotations().sortBy(_.annotationType.toString) do
        print(s" @${a.annotationType.getName}")
      println()
    }
