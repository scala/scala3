inline def c: Int = 2

trait A:
  def f: Unit

class B extends A:
  override inline def f: Unit =
    inline if c == 2 then () else ()
