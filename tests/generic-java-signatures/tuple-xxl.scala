class Test[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23] {
    def foo(tupleXXL: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23)): Unit = {
    }
}

object Test {
    def main(args: Array[String]): Unit = {
        val foo = Class.forName("Test").getDeclaredMethod("foo", classOf[scala.runtime.TupleXXL])
        println(foo.toGenericString)
    }
}
