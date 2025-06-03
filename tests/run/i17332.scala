package foo {

import annotation.static

class MirrorHelpers

object MirrorHelpers:

  @static
  def throwStuff(i: Int): Any = throw new NoSuchElementException(String.valueOf(i))

}

@main def Test =
  try
    foo.MirrorHelpers.throwStuff(23)
    ??? // ko
  catch case ex: NoSuchElementException if ex.getMessage == "23" => () // ok
