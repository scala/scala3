//> using options -Wunused:imports

package some.example:
  package demo:

    import some.example // no warn because enclosing package example is not available as a simple name in some

    object Main {

      def generic[T](x: Any): T = null.asInstanceOf[T]

      def main(args: Array[String]): Unit = {
        generic[example.Util](0)

        import some.example.demo.Main // warn
        println(Main)

        import some.example.demo // warn because enclosing package demo is available as a simple name
        println(demo.Main)
      }
    }

package some.example:

  class Util
