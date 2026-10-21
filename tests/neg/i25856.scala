// https://github.com/scala/scala3/issues/25856
object parser:
  def parse(input: String): Int = ???

import parser.parse.decode // error // error
