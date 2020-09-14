import Nat._

inline def toIntMacro(inline nat: Nat): Int = ${ Macros.toIntImpl('nat) }
inline def ZeroMacro: Zero.type = ${ Macros.natZero }
transparent inline def toNatMacro(inline int: Int): Nat = ${ Macros.toNatImpl('int) }

inline def toIntInline(inline nat: Nat): Int = inline nat match
  case Zero    => 0
  case Succ(n) => toIntInline(n) + 1

object Macros:
  import quoted._

  def toIntImpl(nat: Expr[Nat])(using QuoteContext): Expr[Int] =

    def inner(nat: Expr[Nat], acc: Int): Int = nat match
      case '{ Succ($nat) } => inner(nat, acc + 1)
      case '{ Zero       } => acc

    Expr(inner(nat, 0))

  def natZero(using QuoteContext): Expr[Nat.Zero.type] = '{Zero}

  def toNatImpl(int: Expr[Int])(using QuoteContext): Expr[Nat] =

    // it seems even with the bound that the arg will always widen to Expr[Nat] unless explicit

    def inner[N <: Nat: Type](int: Int, acc: Expr[N]): Expr[Nat] = int match
      case 0 => acc
      case n => inner[Succ[N]](n - 1, '{Succ($acc)})

    val Const(i) = int
    require(i >= 0)
    inner[Zero.type](i, '{Zero})
