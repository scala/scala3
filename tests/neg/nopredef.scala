import Predef.{assert => _}
import dotty.DottyPredef.{assert => _}

object Test {
  assert("asdf" == "asdf") // error: not found assert
}
