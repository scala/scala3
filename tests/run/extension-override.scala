class A with
  extension (s: String)
    def len: Int = s.length

object B extends A with
  extension (s: String)
    override def len: Int = s.length + 1

@main def Test =
  import B._
  assert("abc".len == 4)

