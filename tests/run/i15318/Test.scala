object Test:
  def main(args: Array[String]): Unit =
    val clasz = classOf[TestBeanProperty]

    for (m <- clasz.getDeclaredFields.sortBy(_.getName)) {
      m.setAccessible(true)
      println(s"inspecting field ${m.getName}")
      for a <- m.getAnnotations().sortBy(_.toString) do
        println(a)
    }

    for (m <- clasz.getDeclaredMethods.sortBy(_.getName)) {
      m.setAccessible(true)
      println(s"inspecting method ${m.getName}")
      for a <- m.getAnnotations().sortBy(_.toString) do
        println(a)
    }

    for c <- clasz.getDeclaredConstructors.sortBy(_.getName) do
      c.setAccessible(true)
      println(s"inspecting constructor ${c.getName}")
      for p <- c.getParameters.sortBy(_.getName) do
        println(s"inspecting param ${p.getName}")
        for a <- p.getAnnotations.sortBy(_.toString) do
          println(s"annotation $a")
