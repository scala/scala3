package dotty.tools
package dotc
package typer

// Modelling the decision in IsFullyDefined
object InstantiateModel:
  enum LB { case NN; case LL; case L1 }; import LB.*
  enum UB { case AA; case UU; case U1 }; import UB.*
  enum Decision { case Min; case Max; case ToMax; case Skip; case Fail }; import Decision.*

  // NN/AA = Nothing/Any
  // LL/UU = the original bounds, on the type parameter
  // L1/U1 = the constrained bounds, on the type variable
  // ToMax = delayed maximisation, via addition to toMaximize
  // Skip  = minimisedSelected "hold off instantiating"
  // Fail  = IfBottom.fail's bail option

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

  def instDecision(lb: LB, ub: UB, v: Int, ifBottom: IfBottom, min: Boolean) = (lb, ub) match
    case (L1, AA) => Min
    case (L1, UU) => Min
    case (LL, U1) => Max
    case (NN, U1) => Max

    case (L1, U1) => if min then Min else pickVar(v, Min, Min, ToMax)
    case (LL, UU) => if min then Min else pickVar(v, Min, Min, ToMax)
    case (LL, AA) => if min then Min else pickVar(v, Min, Min, ToMax)

    case (NN, UU) => ifBottom match
      case _ if min      => Max
      case IfBottom.ok   => pickVar(v, Min, ToMax, ToMax)
      case IfBottom.fail => pickVar(v, Fail, Fail, ToMax)
      case IfBottom.flip => ToMax

    case (NN, AA) => ifBottom match
      case _ if min      => Skip
      case IfBottom.ok   => pickVar(v, Min, Min, ToMax)
      case IfBottom.fail => pickVar(v, Fail, Fail, ToMax)
      case IfBottom.flip => ToMax

  def pickVar[A](v: Int, cov: A, inv: A, con: A) =
    if v > 0 then cov else if v == 0 then inv else con
