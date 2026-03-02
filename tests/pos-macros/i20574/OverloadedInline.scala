import Macros.*

object OverloadedInline{

    A()
    inline def overloaded_inline[T]: Unit = {
        overloaded_inline[T](0)
    }

    inline def overloaded_inline[T](dummy: Int): Unit = {
        val crash = B[T]
    }
}
