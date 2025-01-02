//> using options -Werror -Wunused:all

trait Builder {
  def foo(): Unit
}

def `i18366` =
  val builder: Builder = ???
  import builder.{foo => bar}
  bar()

import java.io.DataOutputStream

val buffer: DataOutputStream = ???

import buffer.{write => put}

def `i17315` =
  put(0: Byte)
