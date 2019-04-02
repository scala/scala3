object Test extends App {
  inline val b = 33

  locally {
    def f(): Int = b + 1
    val x1 = List(b, 33, 5.5)      ; x1: List[Double] // b is an inline val
    val x2 = List(f(), 33, 5.5)    ; x2: List[AnyVal] // f() is not a constant
    val x3 = List(5, 11L)          ; x3: List[Long]
    val x4 = List(5, 11L, 5.5)     ; x4: List[AnyVal] // Long and Double found
    val x5 = List(1.0f, 2)         ; x5: List[Float]
    val x6 = List(1.0f, 1234567890); x6: List[AnyVal] // loss of precision
    val x7 = List(b, 33, 'a')      ; x7: List[Char]
    val x8 = List(5.toByte, 11)    ; x8: List[Byte]

    val x9: List[AnyVal] = List(1.0f, 0)
    assert(x9(0).getClass == classOf[java.lang.Float])
    assert(x9(1).getClass == classOf[java.lang.Float]) // expected type not fully defined, since `List` is covariant
    val x10 = List[Any](1.0f, 0)
    assert(x10(0).getClass == classOf[java.lang.Float])
    assert(x10(1).getClass == classOf[java.lang.Integer])
  }

  locally {
    def f(): Int = b + 1
    val x1 = Array(b, 33, 5.5)      ; x1: Array[Double] // b is an inline val
    val x2 = Array(f(), 33, 5.5)    ; x2: Array[AnyVal] // f() is not a constant
    val x3 = Array(5, 11L)          ; x3: Array[Long]
    val x4 = Array(5, 11L, 5.5)     ; x4: Array[AnyVal] // Long and Double found
    val x5 = Array(1.0f, 2)         ; x5: Array[Float]
    val x6 = Array(1.0f, 1234567890); x6: Array[AnyVal] // loss of precision
    val x7 = Array(b, 33, 'a')      ; x7: Array[Char]
    val x8 = Array(5.toByte, 11)    ; x8: Array[Byte]

    val x9: Array[AnyVal] = Array(1.0f, 0)
    assert(x9(0).getClass == classOf[java.lang.Float])
    assert(x9(1).getClass == classOf[java.lang.Integer]) // expected type fully defined since Array is nonvariant
    val x10 = Array[Any](1.0f, 0)
    assert(x10(0).getClass == classOf[java.lang.Float])
    assert(x10(1).getClass == classOf[java.lang.Integer])
  }

  locally {
    def f(): Int = b + 1
    val x1 = if (true) b else if (true) 33 else 5.5    ; x1: Double // b is an inline val
    val x2 = if (true) f() else if (true) 33 else 5.5  ; x2: AnyVal // f() is not a constant
    val x3 = if (true) 5 else 11L                      ; x3: Long
    val x4 = if (true) 5 else if (true) 11L else 5.5   ; x4: AnyVal // Long and Double found
    val x5 = if (true) 1.0f else 2                     ; x5: Float
    val x6 = if (true) 1.0f else 1234567890            ; x6: AnyVal // loss of precision
    val x7 = if (true) b else if (true) 33 else 'a'    ; x7: Char
    val x8 = if (true) 5.toByte else 11                ; x8: Byte
  }
}