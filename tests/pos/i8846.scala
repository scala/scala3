package example

class X(a: => Any)

class XImpl(
  param: Int
) extends X({
    def helper(): AutoCloseable = new AutoCloseable {
      def close() = println(param)
    }
    helper()
  })
