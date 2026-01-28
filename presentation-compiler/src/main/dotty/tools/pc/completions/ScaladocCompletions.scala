package dotty.tools.pc.completions

import scala.meta.pc.PresentationCompilerConfig

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameKinds.*
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Types.ExprType
import dotty.tools.dotc.core.Types.MethodOrPoly
import dotty.tools.dotc.util.SourcePosition

object ScaladocCompletions:
  // The indent for gutter asterisks aligned in column three.
  // |/**
  // |  *
  private val scaladocIndent = "  "

  def contribute(
      pos: SourcePosition,
      text: String,
      config: PresentationCompilerConfig
  )(using Context): List[CompletionValue] =
    def buildText(
        params: List[String],
        hasReturnValue: Boolean,
        indent: String
    ): String =
      val builder = new StringBuilder()
      builder.append("\n")
      builder.append(s"${indent}*")
      if config.isCompletionSnippetsEnabled() then builder.append(" $0\n")
      else builder.append("\n")

      if params.nonEmpty || hasReturnValue then builder.append(s"$indent*\n")

      params.foreach(param => builder.append(s"$indent* @param $param\n"))
      if hasReturnValue then builder.append(s"$indent* @return\n")
      builder.append(s"$indent*/")

      builder.toString()
    end buildText

    val ctx = summon[Context]
    val (numIndent, shouldTabIndent) = CompletionPos.inferIndent(
      ctx.source.lineToOffset(pos.line),
      text
    )
    val indentChar = if shouldTabIndent then "\t" else " "
    val necessaryIndent = indentChar * numIndent
    val indent = necessaryIndent + scaladocIndent

    val finder = new AssociatedMemberDefFinder(pos)
    val root = ctx.compilationUnit.tpdTree
    finder.findAssociatedDef(root) match
      case None =>
        val newText = buildText(Nil, false, indent)
        List(CompletionValue.document("/** */", newText, "Scaladoc Comment"))
      case Some(defn) =>
        val params = defn.getParamss

        val hasReturnValue = defn.symbol.info match
          case tpe: (MethodOrPoly | ExprType) =>
            if defn.symbol.isConstructor then false
            else !(tpe.finalResultType =:= ctx.definitions.UnitType)
          case _ => false

        val newText = buildText(params, hasReturnValue, indent)
        List(CompletionValue.document("/** */", newText, "Scaladoc Comment"))
    end match

  end contribute

  def isScaladocCompletion(pos: SourcePosition, text: String): Boolean =
    pos.point >= 3 &&
      text.charAt(pos.point - 3) == '/' &&
      text.charAt(pos.point - 2) == '*' &&
      text.charAt(pos.point - 1) == '*'

  class AssociatedMemberDefFinder(pos: SourcePosition) extends TreeTraverser:
    private var associatedDef: Option[MemberDef] = None

    override def traverse(tree: Tree)(using Context): Unit = tree match
      case d: (TypeDef | DefDef | ValDef)
          if d.sourcePos.exists && d.sourcePos.span.isSourceDerived &&
            pos.startLine + 1 == d.sourcePos.startLine &&
            !d.symbol.is(Synthetic) =>
        val isCandidate = associatedDef match
          case Some(current) =>
            d.sourcePos.start <= current.sourcePos.start
          case None => true
        if isCandidate then associatedDef = Some(d)
      case _ => traverseChildren(tree)

    def findAssociatedDef(root: Tree)(using Context): Option[MemberDef] =
      associatedDef = None
      traverse(root)
      associatedDef
  end AssociatedMemberDefFinder

  extension (defn: MemberDef)
    private def getParamss(using ctx: Context): List[String] =
      defn match
        case defdef: DefDef =>
          val extensionParam =
            if defdef.symbol.isAllOf(ExtensionMethod) then
              defdef.symbol.extensionParam
            else NoSymbol
          defdef.trailingParamss.flatten.collect {
            case param
                if !param.symbol.isOneOf(Synthetic) &&
                  !param.name.is(ContextBoundParamName) &&
                  param.symbol != extensionParam =>
              param.name.show
          }
        case clazz: TypeDef =>
          clazz.symbol.primaryConstructor.rawParamss.flatten.collect {
            case param
                if !param.is(Synthetic) &&
                  !param.isTypeParam &&
                  !param.name.is(ContextBoundParamName) =>
              param.name.show
          }
        case other =>
          Nil

end ScaladocCompletions
