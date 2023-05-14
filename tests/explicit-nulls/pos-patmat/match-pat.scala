// Ensure we don't get "the type test for argType cannot be checked at runtime" warning

class Symbol {
  type ThisName
}

type TermSymbol = Symbol { type ThisName = String }

type TermSymbolOrNull = TermSymbol | Null

def testSimple =
  val x: Symbol | Null = ???
  x match
    case key: Symbol => 1
    case null => 0

def testWithRefinedType =
  val x: TermSymbol | Null = ???
  x match
    case key: TermSymbol => 1
    case null => 0

def testWithAlias =
  val x: TermSymbolOrNull = ???
  x match
    case key: TermSymbol => 1
    case null => 0
