package org.scalajs.testsuite.compiler

import org.junit.Assert.*
import org.junit.Test

class EnumTestScala3:
  import EnumTestScala3.*

  @Test def testColor1(): Unit =
    import EnumTestScala3.{Color1 as Color}

    def code(c: Color): Character = c match
      case Color.Red   => 'R'
      case Color.Green => 'G'
      case Color.Blue  => 'B'

    assert(Color.Red.ordinal == 0)
    assert(Color.Green.ordinal == 1)
    assert(Color.Blue.ordinal == 2)
    assert(Color.Red.productPrefix == "Red")
    assert(Color.Green.productPrefix == "Green")
    assert(Color.Blue.productPrefix == "Blue")
    assert(Color.valueOf("Red") == Color.Red)
    assert(Color.valueOf("Green") == Color.Green)
    assert(Color.valueOf("Blue") == Color.Blue)
    assert(Color.valueOf("Blue") != Color.Red)
    assert(Color.valueOf("Blue") != Color.Green)
    assert(Color.values(0) == Color.Red)
    assert(Color.values(1) == Color.Green)
    assert(Color.values(2) == Color.Blue)
    assert(code(Color.Red)   == 'R')
    assert(code(Color.Green) == 'G')
    assert(code(Color.Blue)  == 'B')

  end testColor1

  @Test def testColor2(): Unit =  // copied from `color1`
    import EnumTestScala3.{Color2 as Color}

    def code(c: Color): Character = c match
      case Color.Red   => 'R'
      case Color.Green => 'G'
      case Color.Blue  => 'B'

    assert(Color.Red.ordinal == 0)
    assert(Color.Green.ordinal == 1)
    assert(Color.Blue.ordinal == 2)
    assert(Color.Red.productPrefix == "Red")
    assert(Color.Green.productPrefix == "Green")
    assert(Color.Blue.productPrefix == "Blue")
    assert(Color.valueOf("Red") == Color.Red)
    assert(Color.valueOf("Green") == Color.Green)
    assert(Color.valueOf("Blue") == Color.Blue)
    assert(Color.valueOf("Blue") != Color.Red)
    assert(Color.valueOf("Blue") != Color.Green)
    assert(Color.values(0) == Color.Red)
    assert(Color.values(1) == Color.Green)
    assert(Color.values(2) == Color.Blue)
    assert(code(Color.Red)   == 'R')
    assert(code(Color.Green) == 'G')
    assert(code(Color.Blue)  == 'B')

  end testColor2

  @Test def testCurrency1(): Unit =
    import EnumTestScala3.{Currency1 as Currency}

    def code(c: Currency): String = c match
      case Currency.Dollar    => "USD"
      case Currency.SwissFanc => "CHF"
      case Currency.Euro      => "EUR"

    assert(Currency.Dollar.ordinal == 0)
    assert(Currency.SwissFanc.ordinal == 1)
    assert(Currency.Euro.ordinal == 2)
    assert(Currency.Dollar.productPrefix == "Dollar")
    assert(Currency.SwissFanc.productPrefix == "SwissFanc")
    assert(Currency.Euro.productPrefix == "Euro")
    assert(Currency.valueOf("Dollar") == Currency.Dollar)
    assert(Currency.valueOf("SwissFanc") == Currency.SwissFanc)
    assert(Currency.valueOf("Euro") == Currency.Euro)
    assert(Currency.valueOf("Euro") != Currency.Dollar)
    assert(Currency.valueOf("Euro") != Currency.SwissFanc)
    assert(Currency.values(0) == Currency.Dollar)
    assert(Currency.values(1) == Currency.SwissFanc)
    assert(Currency.values(2) == Currency.Euro)
    assert(Currency.Dollar.dollarValue == 1.00)
    assert(Currency.SwissFanc.dollarValue == 1.09)
    assert(Currency.Euro.dollarValue == 1.18)
    assert(code(Currency.Dollar)    == "USD")
    assert(code(Currency.SwissFanc) == "CHF")
    assert(code(Currency.Euro)      == "EUR")


  end testCurrency1

  @Test def testCurrency2(): Unit = // copied from `testCurrency1`
    import EnumTestScala3.{Currency2 as Currency}

    def code(c: Currency): String = c match
      case Currency.Dollar    => "USD"
      case Currency.SwissFanc => "CHF"
      case Currency.Euro      => "EUR"

    assert(Currency.Dollar.ordinal == 0)
    assert(Currency.SwissFanc.ordinal == 1)
    assert(Currency.Euro.ordinal == 2)
    assert(Currency.Dollar.productPrefix == "Dollar")
    assert(Currency.SwissFanc.productPrefix == "SwissFanc")
    assert(Currency.Euro.productPrefix == "Euro")
    assert(Currency.valueOf("Dollar") == Currency.Dollar)
    assert(Currency.valueOf("SwissFanc") == Currency.SwissFanc)
    assert(Currency.valueOf("Euro") == Currency.Euro)
    assert(Currency.valueOf("Euro") != Currency.Dollar)
    assert(Currency.valueOf("Euro") != Currency.SwissFanc)
    assert(Currency.values(0) == Currency.Dollar)
    assert(Currency.values(1) == Currency.SwissFanc)
    assert(Currency.values(2) == Currency.Euro)
    assert(Currency.Dollar.dollarValue == 1.00)
    assert(Currency.SwissFanc.dollarValue == 1.09)
    assert(Currency.Euro.dollarValue == 1.18)
    assert(code(Currency.Dollar)    == "USD")
    assert(code(Currency.SwissFanc) == "CHF")
    assert(code(Currency.Euro)      == "EUR")

  end testCurrency2

  @Test def testOpt(): Unit =

    def encode[T <: AnyVal](t: Opt[T]): T | Null = t match
      case Opt.Sm(t) => t
      case Opt.Nn    => null

    assert(Opt.Sm(1).ordinal == 0)
    assert(Opt.Nn.ordinal == 1)
    assert(Opt.Sm(1).productPrefix == "Sm")
    assert(Opt.Nn.productPrefix == "Nn")
    assert(Opt.Sm("hello").value == "hello")
    assert(encode(Opt.Sm(23)) == 23)
    assert(encode(Opt.Nn)     == null)

  end testOpt

object EnumTestScala3:

  enum Color1 derives CanEqual:
    case Red, Green, Blue

  enum Color2 extends java.lang.Enum[Color2] derives CanEqual:
    case Red, Green, Blue

  // test "non-simple" cases with anonymous subclasses
  enum Currency1(val dollarValue: Double) derives CanEqual:
    case Dollar    extends Currency1(1.0)
    case SwissFanc extends Currency1(1.09)
    case Euro      extends Currency1(1.18)

  enum Currency2(val dollarValue: Double) extends java.lang.Enum[Currency2] derives CanEqual:
    case Dollar    extends Currency2(1.0)
    case SwissFanc extends Currency2(1.09)
    case Euro      extends Currency2(1.18)

  enum Opt[+T]:
    case Sm[+T1](value: T1) extends Opt[T1]
    case Nn                 extends Opt[Nothing]

end EnumTestScala3
