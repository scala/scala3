package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait PrintingOps extends Core with

  object Printers with

    class Printer with
      def println(msg: => String): Unit = System.out.println(msg)

    val pickling = new Printer with
      override def println(msg: => String): Unit = internal.pickling_println(msg)
