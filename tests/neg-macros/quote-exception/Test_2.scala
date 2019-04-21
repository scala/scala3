import Macro_1._

object Test_2 {
  foo(true)
  foo(false) // error: Failed to evaluate inlined quote. Caused by: an implementation is missing
}
