package dotty.tools
package dotc
package typer

// Modelling the decision in IsFullyDefined
object InstantiateModel:
  enum LB  { case NN; case LL; case L1                             }; import LB.*
  enum UB  { case AA; case UU; case U1                             }; import UB.*
  enum Var { case V; case NotV                                     }; import Var.*
  enum MSe { case M; case NotM                                     }; import MSe.*
  enum Bot { case Fail; case Ok; case Flip                         }; import Bot.*
  enum Act { case Min; case Max; case ToMax; case Skip; case False }; import Act.*

  // NN/AA = Nothing/Any
  // LL/UU = the original bounds, on the type parameter
  // L1/U1 = the constrained bounds, on the type variable
  // V     = variance >= 0 ("non-contravariant")
  // MSe   = minimisedSelected
  // Bot   = IfBottom
  // ToMax = delayed maximisation, via addition to toMaximize
  // Skip  = minimisedSelected "hold off instantiating"
  // False = return false

  // there are 9 combinations:
  // # | LB | UB | d | // d = direction
  // --+----+----+---+
  // 1 | L1 | AA | - |       L1 <: T
  // 2 | L1 | UU | - |       L1 <: T       <: UU
  // 3 | LL | U1 | + | LL <:       T <: U1
  // 4 | NN | U1 | + |             T <: U1
  // 5 | L1 | U1 | 0 |       L1 <: T <: U1
  // 6 | LL | UU | 0 | LL <:       T       <: UU
  // 7 | LL | AA | 0 | LL <:       T
  // 8 | NN | UU | 0 |             T       <: UU
  // 9 | NN | AA | 0 |             T

  def decide(lb: LB, ub: UB, v: Var, bot: Bot, m: MSe): Act = (lb, ub) match
    case (L1, AA) => Min
    case (L1, UU) => Min
    case (LL, U1) => Max
    case (NN, U1) => Max

    case (L1, U1) => if m==M || v==V then Min else ToMax
    case (LL, UU) => if m==M || v==V then Min else ToMax
    case (LL, AA) => if m==M || v==V then Min else ToMax

    case (NN, UU) => bot match
      case _    if m==M => Max
    //case Ok   if v==V => Min   // removed, i14218 fix
      case Fail if v==V => False
      case _            => ToMax

    case (NN, AA) => bot match
      case _    if m==M => Skip
      case Ok   if v==V => Min
      case Fail if v==V => False
      case _            => ToMax
