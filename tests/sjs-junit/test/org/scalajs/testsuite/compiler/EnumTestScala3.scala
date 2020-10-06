package org.scalajs.testsuite.compiler

import org.junit.Assert._
import org.junit.Test

object EnumTestScala3:

  enum Color1 derives Eql:
    case Red, Green, Blue

  enum Color2 extends java.lang.Enum[Color2] derives Eql:
    case Red, Green, Blue

  def color1(): Unit =
    import EnumTestScala3.{Color1 => Color}
    assert(Color.Red.ordinal == 0)
    assert(Color.Green.ordinal == 1)
    assert(Color.Blue.ordinal == 2)
    assert(Color.Red.enumLabel == "Red")
    assert(Color.Green.enumLabel == "Green")
    assert(Color.Blue.enumLabel == "Blue")
    assert(Color.valueOf("Red") == Color.Red)
    assert(Color.valueOf("Green") == Color.Green)
    assert(Color.valueOf("Blue") == Color.Blue)
    assert(Color.valueOf("Blue") != Color.Red)
    assert(Color.valueOf("Blue") != Color.Green)
    assert(Color.values(0) == Color.Red)
    assert(Color.values(1) == Color.Green)
    assert(Color.values(2) == Color.Blue)
  end color1

  def color2(): Unit = // copied from `color1`
    import EnumTestScala3.{Color2 => Color}
    assert(Color.Red.ordinal == 0)
    assert(Color.Green.ordinal == 1)
    assert(Color.Blue.ordinal == 2)
    assert(Color.Red.enumLabel == "Red")
    assert(Color.Green.enumLabel == "Green")
    assert(Color.Blue.enumLabel == "Blue")
    assert(Color.valueOf("Red") == Color.Red)
    assert(Color.valueOf("Green") == Color.Green)
    assert(Color.valueOf("Blue") == Color.Blue)
    assert(Color.valueOf("Blue") != Color.Red)
    assert(Color.valueOf("Blue") != Color.Green)
    assert(Color.values(0) == Color.Red)
    assert(Color.values(1) == Color.Green)
    assert(Color.values(2) == Color.Blue)
  end color2

  // test "non-simple" cases with anonymous subclasses
  enum Currency1(val dollarValue: Double) derives Eql:
    case Dollar    extends Currency1(1.0)
    case SwissFanc extends Currency1(1.09)
    case Euro      extends Currency1(1.18)

  enum Currency2(val dollarValue: Double) extends java.lang.Enum[Currency2] derives Eql:
    case Dollar    extends Currency2(1.0)
    case SwissFanc extends Currency2(1.09)
    case Euro      extends Currency2(1.18)

  def currency1(): Unit =
    import EnumTestScala3.{Currency1 => Currency}
    assert(Currency.Dollar.ordinal == 0)
    assert(Currency.SwissFanc.ordinal == 1)
    assert(Currency.Euro.ordinal == 2)
    assert(Currency.Dollar.enumLabel == "Dollar")
    assert(Currency.SwissFanc.enumLabel == "SwissFanc")
    assert(Currency.Euro.enumLabel == "Euro")
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
  end currency1

  def currency2(): Unit = // copied from `currency1`
    import EnumTestScala3.{Currency2 => Currency}
    assert(Currency.Dollar.ordinal == 0)
    assert(Currency.SwissFanc.ordinal == 1)
    assert(Currency.Euro.ordinal == 2)
    assert(Currency.Dollar.enumLabel == "Dollar")
    assert(Currency.SwissFanc.enumLabel == "SwissFanc")
    assert(Currency.Euro.enumLabel == "Euro")
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
  end currency2

  enum Opt[+T]:
    case Sm[+T1](value: T1) extends Opt[T1]
    case Nn                 extends Opt[Nothing]

  def opt(): Unit =
    assert(Opt.Sm(1).ordinal == 0)
    assert(Opt.Nn.ordinal == 1)
    assert(Opt.Sm(1).enumLabel == "Sm")
    assert(Opt.Nn.enumLabel == "Nn")
    assert(Opt.valueOf("Nn") == Opt.Nn)
    assert(Opt.values(0) == Opt.Nn)
    assert(Opt.Sm("hello").value == "hello")
  end opt

class EnumTestScala3:
  import EnumTestScala3._
  @Test def testColor1(): Unit = color1()
  @Test def testColor2(): Unit = color2()
  @Test def testCurrency1(): Unit = currency1()
  @Test def testCurrency2(): Unit = currency2()
  @Test def testOpt(): Unit = opt()
