package pprint

trait TPrintLowPri{
  inline given default[T]: TPrint[T] = ${ TPrintLowPri.typePrintImpl[T] }
}

object TPrintLowPri{

  import scala.quoted._

  sealed trait WrapType
  object WrapType{
    case object NoWrap extends WrapType
    case object Infix extends WrapType
    case object Tuple extends WrapType
  }

  val functionTypes = Range.inclusive(0, 22).map(i => s"scala.Function$i").toSet
  val tupleTypes = Range.inclusive(0, 22).map(i => s"scala.Tuple$i").toSet

  def typePrintImpl[T](using Quotes, Type[T]): Expr[TPrint[T]] = {

    import quotes.reflect._
    import util._

    def literalColor(s: fansi.Str): fansi.Str = {
      fansi.Color.Green(s)
    }

    def printSymString(s: String) =
      if (s.toString.startsWith("_$")) "_"
      else s.toString.stripSuffix(".type")

    def printBounds(lo: TypeRepr, hi: TypeRepr): fansi.Str = {
      val loTree =
        if (lo =:= TypeRepr.of[Nothing]) None else Some(fansi.Str(" >: ") ++ rec(lo) )
      val hiTree =
        if (hi =:= TypeRepr.of[Any]) None else Some(fansi.Str(" <: ") ++ rec(hi) )
      val underscore = fansi.Str("_")
      loTree.orElse(hiTree).map(underscore ++ _).getOrElse(underscore)
    }

    def printSym(s: String): fansi.Str = literalColor(fansi.Str(s))

    //TODO: We don't currently use this method
    def prefixFor(pre: TypeTree, sym: String): fansi.Str = {
      // Depending on what the prefix is, you may use `#`, `.`
      // or even need to wrap the prefix in parentheses
      val sep = pre match {
        case x if x.toString.endsWith(".type") =>
          rec(pre.tpe) ++ "."
      }
      sep ++ printSym(sym)
    }


    def printArgs(args: List[TypeRepr]): fansi.Str = {
      fansi.Str("[") ++ printArgs0(args) ++ "]"
    }

    def printArgs0(args: List[TypeRepr]): fansi.Str = {
      val added = fansi.Str.join(
        args.map {
          case TypeBounds(lo, hi) =>
            printBounds(lo, hi)
          case tpe: TypeRepr =>
            rec(tpe, false)
        },
        sep = ", "
      )
      added
    }


    object RefinedType {
      def unapply(tpe: TypeRepr): Option[(TypeRepr, List[(String, TypeRepr)])] = tpe match {
        case Refinement(p, i, b) =>
          unapply(p).map {
            case (pp, bs) => (pp, (i -> b) :: bs)
          }.orElse(Some((p, (i -> b) :: Nil)))
        case _ => None
      }
    }

    def rec(tpe: TypeRepr, end: Boolean = false): fansi.Str = rec0(tpe)._1
    def rec0(tpe: TypeRepr, end: Boolean = false): (fansi.Str, WrapType) = tpe match {
      case TypeRef(NoPrefix(), sym) =>
        (printSym(sym), WrapType.NoWrap)
        // TODO: Add prefix handling back in once it works!
      case TypeRef(_, sym) =>
        (printSym(sym), WrapType.NoWrap)
      case AppliedType(tpe, args) =>
        if (functionTypes.contains(tpe.typeSymbol.fullName)) {
          (
            if(args.size == 1 ) fansi.Str("() => ") ++ rec(args.last)
            else{
              val (left, wrap) = rec0(args(0))
              if(args.size == 2 && wrap == WrapType.NoWrap){
                left ++ fansi.Str(" => ") ++ rec(args.last)
              }
              else fansi.Str("(") ++ printArgs0(args.dropRight(1)) ++ fansi.Str(") => ") ++ rec(args.last)

            },
            WrapType.Infix
          )

        } else if (tupleTypes.contains(tpe.typeSymbol.fullName))
          (fansi.Str("(") ++ printArgs0(args) ++ fansi.Str(")"), WrapType.Tuple)
        else (printSym(tpe.typeSymbol.name) ++ printArgs(args), WrapType.NoWrap)
      case RefinedType(tpe, refinements) =>
        val pre = rec(tpe)
        lazy val defs = fansi.Str.join(
          refinements.collect {
            case (name, tpe: TypeRepr) =>
              fansi.Str("type " + name + " = ") ++ rec(tpe)
//            case (name, TypeBounds(lo, hi)) =>
//              fansi.Str("type " + name) ++ printBounds(lo, hi) ++ rec(tpe)
          },
          sep = "; "
        )
        (pre ++ (if(refinements.isEmpty) fansi.Str("") else fansi.Str("{") ++ defs ++ "}"), WrapType.NoWrap)
      case AnnotatedType(parent, annot) =>
        (rec(parent, end), WrapType.NoWrap)
      case _ =>
        (fansi.Str(Type.show[T]), WrapType.NoWrap)
    }
    val value: fansi.Str = rec(TypeRepr.of[T])

    '{TPrint.recolor(fansi.Str(${Expr(value.render)}))}
  }
}
