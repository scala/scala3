object Test:
  def main(args: Array[String]): Unit =
    val cls = classOf[MyTable]

    for (m <- cls.getDeclaredFields.sortBy(_.getName)) {
      m.setAccessible(true)
      println(s"inspecting field ${m.getName}")
      for a <- m.getAnnotations().sortBy(_.annotationType.toString) do
        println(a.annotationType)
    }

    for (m <- cls.getDeclaredMethods.sortBy(_.getName)) {
      m.setAccessible(true)
      println(s"inspecting method ${m.getName}")
      for a <- m.getAnnotations().sortBy(_.annotationType.toString) do
        println(a.annotationType)
    }

    for c <- cls.getDeclaredConstructors.sortBy(_.getName) do
      c.setAccessible(true)
      println(s"inspecting constructor ${c.getName}")
      for p <- c.getParameters.sortBy(_.getName) do
        println(s"inspecting param ${p.getName}")
        for a <- p.getAnnotations.sortBy(_.annotationType.toString) do
          println(a.annotationType)
