import Nat.*

 inline def toIntMacro(inline nat: Nat): Int = ${ Macros.toIntImpl('nat) }
 inline def ZeroMacro: Zero.type = ${ Macros.natZero }
 transparent inline def toNatMacro(inline int: Int): Nat = ${ Macros.toNatImpl('int) }

 object Macros:
   import quoted.*

   def toIntImpl(nat: Expr[Nat])(using Quotes): Expr[Int] =

     def inner(nat: Expr[Nat], acc: Int): Int = nat match
       case '{ Succ($nat) } => inner(nat, acc + 1)
       case '{ Zero       } => acc

     Expr(inner(nat, 0))

   def natZero(using Quotes): Expr[Nat.Zero.type] = '{Zero}

   def toNatImpl(int: Expr[Int])(using Quotes): Expr[Nat] =

     // it seems even with the bound that the arg will always widen to Expr[Nat] unless explicit

     def inner[N <: Nat: Type](int: Int, acc: Expr[N]): Expr[Nat] = int match
       case 0 => acc
       case n => inner[Succ[N]](n - 1, '{Succ($acc)})

     val i = int.valueOrError
     require(i >= 0)
     inner[Zero.type](i, '{Zero})
