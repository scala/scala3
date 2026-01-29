package dotty.tools.pc.completions

import scala.annotation.tailrec
import scala.util.Try

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.*
import dotty.tools.pc.ApplyArgsExtractor
import dotty.tools.pc.ApplyExtractor
import dotty.tools.pc.IndexedContext
import dotty.tools.pc.ParamSymbol
import dotty.tools.pc.utils.InteractiveEnrichments.*

object NamedArgCompletions:

  def contribute(
      path: List[Tree],
      untypedPath: List[untpd.Tree],
      indexedContext: IndexedContext,
      clientSupportsSnippets: Boolean
  )(using ctx: Context): List[CompletionValue] =
    path match
      case (ident: Ident) :: ApplyExtractor(app) =>
        contribute(
          ident,
          app,
          indexedContext,
          clientSupportsSnippets
        )
      case (app: Apply) :: _ =>
        /** def foo(aaa: Int, bbb: Int, ccc: Int) = ??? val x = foo( bbb = 123,
         *  ccc = 123,
         *  @@ ) In this case, typed path doesn't contain already provided
         *    arguments
         */
        untypedPath match
          case (ident: Ident) :: (app: Apply) :: _ =>
            contribute(
              ident,
              app,
              indexedContext,
              clientSupportsSnippets
            )
          case _ =>
            Nil
      case _ =>
        Nil
    end match

  private def contribute(
      ident: Ident,
      apply: Apply,
      indexedContext: IndexedContext,
      clientSupportsSnippets: Boolean
  )(using context: Context): List[CompletionValue] =
    def isUselessLiteral(arg: Tree): Boolean =
      arg match
        case Literal(Constant(())) => true // unitLiteral
        case Literal(Constant(null)) => true // nullLiteral
        case _ => false

    val argsAndParams = ApplyArgsExtractor.getArgsAndParams(
      Some(indexedContext),
      apply,
      ident.span
    )

    val allParams = argsAndParams.flatMap { case (baseArgs, baseParams) =>
      val args = baseArgs.filterNot(a => a == ident || isUselessLiteral(a))

      @tailrec
      def isDefaultArg(t: Tree): Boolean = t match
        // default args
        case Ident(name) => name.is(DefaultGetterName)
        // default args for methods defined in object
        case Select(_, name) =>
          name.is(DefaultGetterName)
        // default args in not-first parameter list
        // eg. def m(fst: Int)(snd: Int)(arg1: Int, arg2: Int = 123) = ???
        case Apply(fun, _) => isDefaultArg(fun)
        case _ => false

      val isNamed: Set[Name] = args.iterator
        .zip(baseParams.iterator)
        // filter out synthesized args and default arg getters
        .filterNot {
          case (arg, _) if arg.symbol.denot.is(Flags.Synthetic) => true
          case (arg, _) => isDefaultArg(arg)
        }
        .map {
          case (NamedArg(name, _), _) => name
          case (_, param) => param.name
        }
        .toSet

      baseParams.filterNot(param =>
        isNamed(param.name) ||
          param.symbol.denot.is(
            Flags.Synthetic
          ) // filter out synthesized param, like evidence
      )
    }

    val prefix =
      ident.name.toString
        .replace(Cursor.value, "")
        .nn

    val params: List[ParamSymbol] =
      allParams
        .filter(param => param.name.startsWith(prefix))
        .distinctBy(sym => (sym.name, sym.info))

    val completionSymbols = indexedContext.scopeSymbols
    def matchingTypesInScope(paramType: Type): List[String] =
      if paramType != defn.AnyType then
        completionSymbols
          .collect {
            case sym
                if sym.info <:< paramType && sym.isTerm && !sym.info.isErroneous && !sym.info.isNullType && !sym.info.isNothingType && !sym
                  .is(Flags.Method) && !sym.is(Flags.Synthetic) =>
              sym.decodedName
          }
          .filter(name => name != "Nil" && name != "None")
          .sorted
      else Nil

    def findDefaultValue(param: ParamSymbol): String =
      val matchingType = matchingTypesInScope(param.info)
      if matchingType.size == 1 then s":${matchingType.head}"
      else if matchingType.size > 1 then s"|???,${matchingType.mkString(",")}|"
      else ":???"

    def fillAllFields(): List[CompletionValue] =
      val suffix = "autofill"
      def shouldShow =
        allParams.exists(param => param.name.startsWith(prefix))
      def isExplicitlyCalled = suffix.startsWith(prefix)
      def hasParamsToFill = allParams.count(!_.symbol.is(Flags.HasDefault)) > 1
      if clientSupportsSnippets && argsAndParams.length == 1 && (shouldShow || isExplicitlyCalled) && hasParamsToFill
      then
        val editText = allParams.zipWithIndex
          .collect {
            case (param, index) if !param.symbol.is(Flags.HasDefault) =>
              s"${param.nameBackticked.replace("$", "$$")} = $${${index + 1}${findDefaultValue(param)}}"
          }
          .mkString(", ")
        val labelText = allParams
          .collect {
            case param if !param.symbol.is(Flags.HasDefault) =>
              s"${param.nameBackticked.replace("$", "$$")} = ???"
          }
          .mkString(", ")
        List(
          CompletionValue.Autofill(
            editText,
            labelText
          )
        )
      else List.empty

    def findPossibleDefaults(): List[CompletionValue] =
      params.flatMap { param =>
        val allMembers = matchingTypesInScope(param.info)
        allMembers.map { memberName =>
          val editText =
            param.nameBackticked + " = " + memberName + " "
          CompletionValue.namedArg(
            label = editText,
            param
          )
        }
      }

    params.map(p =>
      CompletionValue.namedArg(
        s"${p.nameBackticked} = ",
        p
      )
    ) ::: findPossibleDefaults() ::: fillAllFields()
  end contribute

end NamedArgCompletions
