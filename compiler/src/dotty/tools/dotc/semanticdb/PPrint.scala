package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.{semanticdb => s}

import scala.collection.mutable
import dotty.tools.dotc.semanticdb.Scala3.{_, given}
import SymbolInformation.Kind._

class SymbolInfomationPrinter (symtab: PrinterSymtab):
  val notes = InfoNotes()
  val infoPrinter = InfoPrinter(notes)

  def pprintSymbolInformation(info: SymbolInformation): String =
    val sb = new StringBuilder()
    sb.append(info.symbol).append(" => ")
    sb.append(infoPrinter.pprint(info))
    sb.toString

  class InfoNotes:
    private val noteSymtab = mutable.Map[String, SymbolInformation]()
    def enter(info: SymbolInformation) =
      if (symtab.info(info.symbol).isEmpty && info.kind != UNKNOWN_KIND)
        noteSymtab(info.symbol) = info

    def visit(sym: String): SymbolInformation =
      val symtabInfo = noteSymtab.get(sym).orElse(symtab.info(sym))
      symtabInfo.getOrElse {
        val displayName = if sym.isGlobal then sym.desc.value else sym
        SymbolInformation(symbol = sym, displayName = displayName)
      }

  class InfoPrinter(notes: InfoNotes) {
    private enum SymbolStyle:
      case Reference, Definition
    def pprint(info: SymbolInformation): String =
      val sb = new StringBuilder()
      sb.append(accessString(info.access))
      if info.isAbstract then sb.append("abstract ")
      if info.isFinal then sb.append("final ")
      if info.isSealed then sb.append("sealed ")
      if info.isImplicit then sb.append("implicit ")
      if info.isLazy then sb.append("lazy ")
      if info.isCase then sb.append("case ")
      if info.isCovariant then sb.append("covariant ")
      if info.isContravariant then sb.append("contravariant ")
      if info.isVal then sb.append("val ")
      if info.isVar then sb.append("var ")
      if info.isStatic then sb.append("static ")
      if info.isPrimary then sb.append("primary ")
      if info.isEnum then sb.append("enum ")
      if info.isDefault then sb.append("default ")
      if info.isGiven then sb.append("given ")
      if info.isInline then sb.append("inline ")
      if info.isOpen then sb.append("open ")
      if info.isTransparent then sb.append("transparent ")
      if info.isInfix then sb.append("infix ")
      if info.isOpaque then sb.append("opaque ")
      info.kind match
        case LOCAL => sb.append("local ")
        case FIELD => sb.append("field ")
        case METHOD => sb.append("method ")
        case CONSTRUCTOR => sb.append("ctor ")
        case MACRO => sb.append("macro ")
        case TYPE => sb.append("type ")
        case PARAMETER => sb.append("param ")
        case SELF_PARAMETER => sb.append("selfparam ")
        case TYPE_PARAMETER => sb.append("typeparam ")
        case OBJECT => sb.append("object ")
        case PACKAGE => sb.append("package ")
        case PACKAGE_OBJECT => sb.append("package object ")
        case CLASS => sb.append("class ")
        case TRAIT => sb.append("trait ")
        case INTERFACE => sb.append("interface ")
        case UNKNOWN_KIND | Unrecognized(_) => sb.append("unknown ")
      sb.append(s"${info.displayName}${info.prefixBeforeTpe}${pprint(info.signature)}")
      sb.toString

    private def pprintDef(info: SymbolInformation) =
      notes.enter(info)
      pprint(info.symbol, SymbolStyle.Definition)
    private def pprintRef(sym: String): String = pprint(sym, SymbolStyle.Reference)
    private def pprintDef(sym: String): String = pprint(sym, SymbolStyle.Definition)
    private def pprint(sym: String, style: SymbolStyle): String =
      val info = notes.visit(sym)
      style match
        case SymbolStyle.Reference =>
          info.displayName
        case SymbolStyle.Definition =>
          pprint(info)


    private def pprint(sig: Signature): String =
      sig match
        case ClassSignature(tparams, parents, self, decls) =>
          val sb = new StringBuilder()
          if (tparams.infos.nonEmpty)
            sb.append(tparams.infos.map(pprintDef).mkString("[", ", ", "] "))
          if (parents.nonEmpty)
            sb.append(parents.map(pprint).mkString("extends ", " with ", " "))
          if (self.isDefined || decls.infos.nonEmpty) {
            val selfStr = if (self.isDefined) s"self: ${pprint(self)} =>" else ""
            val declsStr = if (decls.infos.nonEmpty) s"+${decls.infos.length} decls" else ""
            sb.append(s"{ ${selfStr} ${declsStr} }")
          }
          sb.toString
        case MethodSignature(tparams, paramss, res) =>
          val sb = new StringBuilder()
          if (tparams.infos.nonEmpty)
            sb.append(tparams.infos.map(pprintDef).mkString("[", ", ", "]"))
          paramss.foreach { params =>
            val paramsStr = params.infos.map(pprintDef).mkString("(", ", ", ")")
            sb.append(paramsStr)
          }
          sb.append(s": ${pprint(res)}")
          sb.toString
        case TypeSignature(tparams, lo, hi) =>
          val sb = new StringBuilder()
          if (tparams.infos.nonEmpty)
            sb.append(tparams.infos.map(pprintDef).mkString("[", ", ", "]"))
          if (lo == hi) {
            sb.append(s" = ${pprint(lo)}")
          } else {
            lo match
              case TypeRef(Type.Empty, "scala/Nothing#", Nil) => ()
              case lo => sb.append(s" >: ${pprint(lo)}")
            hi match
              case TypeRef(Type.Empty, "scala/Any#", Nil) => ()
              case TypeRef(Type.Empty, "java/lang/Object#", Nil) => ()
              case hi => sb.append(s" <: ${pprint(hi)}")
          }
          sb.toString
        case ValueSignature(tpe) =>
          pprint(tpe)
        case _ =>
          "<?>"

    private def pprint(tpe: Type): String = {
      def prefix(tpe: Type): String = tpe match
        case TypeRef(pre, sym, args) =>
          val preStr = pre match {
            case _: SingleType | _: ThisType | _: SuperType =>
              s"${prefix(pre)}."
            case Type.Empty => ""
            case _ =>
              s"${prefix(pre)}#"
          }
          val argsStr = if (args.nonEmpty) args.map(normal).mkString("[", ", ", "]") else ""
          s"${preStr}${pprintRef(sym)}${argsStr}"
        case SingleType(pre, sym) =>
          pre match {
            case Type.Empty => pprintRef(sym)
            case _ =>
              s"${prefix(pre)}.${pprintRef(sym)}"
          }
        case ThisType(sym) =>
          s"${pprintRef(sym)}.this"
        case SuperType(pre, sym) =>
          s"${prefix(pre)}.super[${pprintRef(sym)}]"
        case ConstantType(const) =>
          pprint(const)
        case IntersectionType(types) =>
          types.map(normal).mkString(" & ")
        case UnionType(types) =>
          types.map(normal).mkString(" | ")
        case WithType(types) =>
          types.map(normal).mkString(" with ")
        case StructuralType(utpe, decls) =>
          val declsStr =
            if (decls.infos.nonEmpty)
              s"{ ${decls.infos.map(pprintDef).mkString("; ")} }"
            else "{}"
          s"${normal(utpe)} ${declsStr}"
        case AnnotatedType(anns, utpe) =>
          s"${normal(utpe)} ${anns.map(pprint).mkString(" ")}"
        case ExistentialType(utpe, decls) =>
          val sdecls = decls.infos.map(_.displayName).mkString("; ")
          val sutpe = normal(utpe)
          s"${sutpe} forSome { ${sdecls} }"
        case UniversalType(tparams, utpe) =>
          val params = tparams.infos.map(_.displayName).mkString("[", ", ", "]")
          val resType = normal(utpe)
          s"${params} => ${resType}"
        case ByNameType(utpe) =>
          s"=> ${normal(utpe)}"
        case RepeatedType(utpe) =>
          s"${normal(utpe)}*"
        case _ =>
          "<?>"

      def normal(tpe: Type): String = tpe match
        case _: SingleType | _: ThisType | _: SuperType =>
          s"${prefix(tpe)}.type"
        case _ =>
          prefix(tpe)
      normal(tpe)
    }

    private def pprint(ann: Annotation): String =
      ann.tpe match {
        case Type.Empty => s"@<?>"
        case tpe => s"@${pprint(tpe)}"
      }

    private def pprint(const: Constant): String = const match {
        case Constant.Empty =>
          "<?>"
        case UnitConstant() =>
          "()"
        case BooleanConstant(true) =>
          "true"
        case BooleanConstant(false) =>
          "false"
        case ByteConstant(value) =>
          value.toByte.toString
        case ShortConstant(value) =>
          value.toShort.toString
        case CharConstant(value) =>
          s"'${value.toChar.toString}'"
        case IntConstant(value) =>
          value.toString
        case LongConstant(value) =>
          s"${value.toString}L"
        case FloatConstant(value) =>
          s"${value.toString}f"
        case DoubleConstant(value) =>
          value.toString
        case StringConstant(value) =>
          "\"" + value + "\""
        case NullConstant() =>
          "null"
      }

    private def accessString(access: Access): String =
      access match
        case Access.Empty => ""
        case _: PublicAccess => ""
        case _: PrivateAccess => "private "
        case _: ProtectedAccess => "protected "
        case _: PrivateThisAccess => "private[this] "
        case _: ProtectedThisAccess => "protected[this] "
        case PrivateWithinAccess(ssym) =>
          s"private[${ssym}] "
        case ProtectedWithinAccess(ssym) =>
          s"protected[${ssym}] "

    extension (scope: Scope)
      private def infos: List[SymbolInformation] =
        if (scope.symlinks.nonEmpty)
          scope.symlinks.map(symbol => SymbolInformation(symbol = symbol)).toList
        else
          scope.hardlinks.toList

    extension (scope: Option[Scope])
      private def infos: List[SymbolInformation] = scope match {
        case Some(s) => s.infos
        case None => Nil
      }
  }
end SymbolInfomationPrinter

extension (info: SymbolInformation)
  def prefixBeforeTpe: String = {
    info.kind match {
      case LOCAL | FIELD | PARAMETER | SELF_PARAMETER | UNKNOWN_KIND | Unrecognized(_) =>
        ": "
      case METHOD | CONSTRUCTOR | MACRO | TYPE | TYPE_PARAMETER | OBJECT | PACKAGE |
          PACKAGE_OBJECT | CLASS | TRAIT | INTERFACE =>
        " "
    }
  }

trait PrinterSymtab:
  def info(symbol: String): Option[SymbolInformation]
object PrinterSymtab:
  def fromTextDocument(doc: TextDocument): PrinterSymtab =
    val map = doc.symbols.map(info => (info.symbol, info)).toMap
    new PrinterSymtab {
      override def info(symbol: String): Option[SymbolInformation] = map.get(symbol)
    }
