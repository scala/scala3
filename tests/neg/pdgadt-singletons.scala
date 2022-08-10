object test {
  trait Tag

  locally {
    val p0: Tag = ???
    val p1: Tag = ???

    p0 match {
      case q0: Tag =>
        val x1: p0.type = q0
        val x2: q0.type = p0
        val x3: p0.type = p1  // error

        p1 match {
          case q1: Tag =>
            val x1: p1.type = q1
            val x2: q1.type = p1
            val x3: p0.type = p1  // error

            q0 match {
              case r: q1.type =>
                val x1: q0.type = q1
                val x2: p0.type = p1
                val x3: q0.type = p1
                val x4: q1.type = p0
            }
        }
    }
  }
}
