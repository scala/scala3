import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._
import scala.collection.mutable

@experimental
class addClass extends MacroAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    tree match
      case DefDef(name, List(TermParamClause(Nil)), tpt, Some(rhs)) =>
        val parents = List(TypeTree.of[Object])
        def decls(cls: Symbol): List[Symbol] =
          List(Symbol.newMethod(cls, "run", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit]), Flags.Static, Symbol.noSymbol))

        // FIXME: missing flags: Final | Module
        // FIXME: how to set the self type?
        val cls = Symbol.newClass(Symbol.spliceOwner, "Baz", parents = parents.map(_.tpe), decls, selfType = None)
        val mod = Symbol.newVal(Symbol.spliceOwner, "Baz", cls.typeRef, Flags.Module | Flags.Lazy | Flags.Final, Symbol.noSymbol)
        val runSym = cls.declaredMethod("run").head

        val runDef = DefDef(runSym, _ => Some(rhs))

        val clsDef = ClassDef(cls, parents, body = List(runDef))

        val newCls = Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil)
        val modVal = ValDef(mod, Some(newCls))

        val newDef = DefDef.copy(tree)(name, List(TermParamClause(Nil)), tpt, Some(Apply(Select(Ref(mod), runSym), Nil)))
        List(modVal, clsDef, newDef)
      case _ =>
        report.error("Annotation only supports `def` with one argument")
        List(tree)
