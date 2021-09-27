package testcode

import scala.quoted.Quotes

object TestMacro {
    private def impl()(using Quotes) = '{ 123 }
    inline def call(): Int = ${ impl() }
}
