import MIPS.*

import deriving.Mirror.SumOf

object MIPS {
  type Labels     = Label | ControlLabel
  type Src        = Register | Constant
  type LabelIds   = ElseLabel | Join
  type Register   = Results | Arguments | Temporaries | SavedValues | Trap | Misc
  type Addresses  = OffsetAddress | Labels
  type Dest       = Addresses | Register
  type Assembler  = ZeroAddr | OneAddr | TwoAddr | ThreeAddr | PseudoZero |
                      PseudoUnary | Labels | Comment
}

enum Results { case V0, V1 }

enum Arguments { case A0, A1, A2, A3 }

enum Temporaries { case T0, T1, T2, T3, T4, T5, T6, T7, T8, T9 }

enum SavedValues { case S0, S1, S2, S3, S4, S5, S6, S7, S8 }

enum Trap { case K0, K1 }

enum Misc { case Zero, Sp, Gp, Fp, Ra }

enum ZeroAddr { case Syscall }

enum OneAddr {
  case Jal(dest: Labels)
  case Jr(dest: Register)
  case J(dest: Labels)
}

enum TwoAddr {
  case Beqz(source: Register, breakTo: Labels)
  case Move(dest: Register, source: Register)
  case Li(dest: Register, source: Constant)
  case Lw(dest: Register, source: Addresses)
  case La(dest: Register, source: Addresses)
  case Sw(source: Register, dest: Addresses)
  case Not(dest: Register, r: Src)
  case Neg(dest: Register, r: Src)
}

enum ThreeAddr {
  case Add(dest: Register, l: Register, r: Src)
  case Sub(dest: Register, l: Register, r: Src)
  case Mul(dest: Register, l: Register, r: Src)
  case Div(dest: Register, l: Register, r: Src)
  case Rem(dest: Register, l: Register, r: Src)
  case Seq(dest: Register, l: Register, r: Src)
  case Sne(dest: Register, l: Register, r: Src)
  case Slt(dest: Register, l: Register, r: Src)
  case Sle(dest: Register, l: Register, r: Src)
  case Sgt(dest: Register, l: Register, r: Src)
  case Sge(dest: Register, l: Register, r: Src)
}

case class ElseLabel(id: Long)
case class Join(id: Long)
case class ControlLabel(id: LabelIds)
case class Label(id: Scoped)
case class OffsetAddress(address: Register, offset: Constant)

case class Constant(value: Int)
case class Scoped(id: Identifier, scope: Long)
case class Identifier(id: String)

enum PseudoZero { case Text, Data }

enum PseudoUnary {
  case Word(size: Constant)
  case Globl(name: Scoped)
  case Asciiz(value: String)
}

case class Comment(msg: String)

object printMips {
  import MIPS.*
  import Misc.*
  import PseudoZero.*
  import PseudoUnary.*
  import ZeroAddr.*
  import OneAddr.*
  import TwoAddr.*
  import ThreeAddr.*

  def apply(nodes: List[Assembler]): Unit = {
    var symbCount = 0L
    val symbols = new scala.collection.mutable.AnyRefMap[Scoped,Long]()

    print(mipsNode(nodes, "  "))

    def mipsNode
      ( nodes: List[Assembler],
        indent: String
      ): String = {
        (for (node <- nodes.view) yield astNode(node, indent)).mkString
      }

    def astNode
      ( node: Assembler,
        indent: String
      ): String = {
        node match {
          case Text =>
            s"$indent.text\n"
          case Comment(msg) =>
            s"$indent#$msg\n"
          case Data =>
            s"$indent.data\n"
          case Globl(Scoped(Identifier(id),_)) =>
            s"$indent.globl $id\n"
          case Label(Scoped(Identifier(i),-1)) =>
            s"$i:\n"
          case Label(s @ Scoped(Identifier(i), id)) =>
            s"${getScopedLabel(s)}: #debug: $i~$id\n"
          case ControlLabel(id) =>
            s"${evalLabels(id)}:\n"
          case Word(Constant(w)) =>
            s"$indent.word $w\n"
          case Syscall =>
            s"${indent}syscall\n"
          case jal: Jal =>
            oneAddr(jal,indent)(_.dest)
          case jr: Jr =>
            oneAddr(jr,indent)(_.dest)
          case j: J =>
            oneAddr(j,indent)(_.dest)
          case li: Li =>
            twoAddr(li,indent)(_.dest,_.source)
          case lw: Lw =>
            twoAddr(lw,indent)(_.dest,_.source)
          case neg: Neg =>
            twoAddr(neg,indent)(_.dest,_.r)
          case not: Not =>
            twoAddr(not,indent)(_.dest,_.r)
          case move: Move =>
            twoAddr(move,indent)(_.dest,_.source)
          case beqz: Beqz =>
            twoAddr(beqz,indent)(_.source,_.breakTo)
          case sw: Sw =>
            twoAddr(sw,indent)(_.source,_.dest)
          case add: Add =>
            threeAddr(add,indent)(_.dest,_.l,_.r)
          case sub: Sub =>
            threeAddr(sub,indent)(_.dest,_.l,_.r)
          case mul: Mul =>
            threeAddr(mul,indent)(_.dest,_.l,_.r)
          case div: Div =>
            threeAddr(div,indent)(_.dest,_.l,_.r)
          case rem: Rem =>
            threeAddr(rem,indent)(_.dest,_.l,_.r)
          case seq: Seq =>
            threeAddr(seq,indent)(_.dest,_.l,_.r)
          case sne: Sne =>
            threeAddr(sne,indent)(_.dest,_.l,_.r)
          case slt: Slt =>
            threeAddr(slt,indent)(_.dest,_.l,_.r)
          case sgt: Sgt =>
            threeAddr(sgt,indent)(_.dest,_.l,_.r)
          case sle: Sle =>
            threeAddr(sle,indent)(_.dest,_.l,_.r)
          case sge: Sge =>
            threeAddr(sge,indent)(_.dest,_.l,_.r)
          case _ => s"${indent}???\n"
        }
      }

    def oneAddr[O]
      ( a: O, indent: String)
      ( r: O => Dest,
      ): String = {
        val name = a.getClass.getSimpleName.toLowerCase
        s"${indent}$name ${rsrc(r(a))}\n"
      }

    def twoAddr[O]
      ( a: O, indent: String)
      ( d: O => Register,
        r: O => Dest | Constant
      ): String = {
        val name = a.getClass.getSimpleName.toLowerCase
        s"${indent}$name ${registers(d(a))}, ${rsrc(r(a))}\n"
      }

    def threeAddr[O]
      ( a: O,
        indent: String )
      ( d: O => Register,
        l: O => Register,
        r: O => Src
      ): String = {
        val name = a.getClass.getSimpleName.toLowerCase
        s"${indent}$name ${registers(d(a))}, ${registers(l(a))}, ${rsrc(r(a))}\n"
      }

    def rsrc(v: Constant | Dest): String = v match {
      case Constant(c) => c.toString
      case Label(Scoped(Identifier(i),-1)) => s"$i"
      case Label(s: Scoped) => getScopedLabel(s)
      case ControlLabel(id) => evalLabels(id)
      case r: Register => registers(r)
      case u => sys.error(s"rsrc: $u")
    }

    def registers(reg: Register): String = reg match {
      case t: Temporaries =>
        printEnum(Temporaries.valueOf, t, "$t")
      case s: SavedValues =>
        printEnum(SavedValues.valueOf, s, "$s")
      case r: Results =>
        printEnum(Results.valueOf, r, "$v")
      case a: Arguments =>
        printEnum(Arguments.valueOf, a, "$a")
      case Ra =>
        "$ra"
      case _ =>
        "$?_"
    }

    def evalLabels(v: LabelIds): String = v match {
      case ElseLabel(c) => s"else$c"
      case Join(ic) => s"join$ic"
    }

    def getScopedId(s: Scoped): Long =
      symbols.getOrElseUpdate(s, { symbCount += 1; symbCount })

    def getScopedLabel(s: Scoped): String =
      "L" + getScopedId(s)

    def printEnum[E: SumOf](e: String => E, t: E, code: String) = {
      val num = summon[SumOf[E]].ordinal(e(t.toString))
        s"$code$num"
    }
  }
}
