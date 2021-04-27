object Exporter {
  object Exportee {
    inline def foo(args: String*): String = args.mkString(" ")

    inline def bar(l: Option[Int])()(i: Int)(b: Boolean, args: String*): String =
      s"$l-$i-$b-${args.mkString("-")}"
  }
  export Exportee._
}

import Exporter.*

@main def Test =
  println(foo("a", "b", "c"))
  println(bar(Some(1))()(2)(true, "a", "b", "c"))
