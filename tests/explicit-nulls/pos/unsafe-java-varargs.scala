import java.nio.file.Paths

import scala.language.unsafeNulls

class S {
  val arg1: Array[String] = ???
  val arg2: Array[String | Null] = ???
  val arg3: Array[String] | Null = ???
  val arg4: Array[String | Null] | Null = ???

  Paths.get("", arg1: _*)
  Paths.get("", arg2: _*)
  Paths.get("", arg3: _*)
  Paths.get("", arg4: _*)
}