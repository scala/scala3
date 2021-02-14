class S {
  locally {
    implicit def f(x: String): Array[String] = ???

    val y1: String = ???
    val y2: String | Null = ???

    y1: Array[String]
    y1: Array[String | Null] // error
    y1: Array[String] | Null
    y1: Array[String | Null] | Null // error

    y2: Array[String] // error
    y2: Array[String | Null] // error
    y2: Array[String] | Null // error
    y2: Array[String | Null] | Null // error
  }

  locally {
    implicit def g(x: Array[String]): String = ???

    val y1: Array[String] = ???
    val y2: Array[String] | Null = ???
    val y3: Array[String | Null] = ???
    val y4: Array[String | Null] | Null = ???

    y1: String
    y2: String // error
    y3: String // error
    y4: String // error

    y1: String | Null
    y2: String | Null // error
    y3: String | Null // error
    y4: String | Null // error
  }

  locally {
    implicit def g(x: Array[String | Null]): String | Null = ???

    val y1: Array[String] = ???
    val y2: Array[String] | Null = ???
    val y3: Array[String | Null] = ???
    val y4: Array[String | Null] | Null = ???

    y1: String // error
    y2: String // error
    y3: String // error
    y4: String // error

    y1: String | Null // error
    y2: String | Null // error
    y3: String | Null
    y4: String | Null // error
  }
}