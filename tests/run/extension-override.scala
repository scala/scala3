class A:
  extension (s: String)
    def len: Int = s.length

object B extends A:
  extension (s: String)
    override def len: Int = s.length + 1

@main def Test =
  import B.*
  assert("abc".len == 4)

