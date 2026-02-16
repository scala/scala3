//> using options -experimental

import scala.annotation.MacroAnnotation
import scala.quoted.*

class implementAFoo extends MacroAnnotation:

    def transform(using Quotes)(tree: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
        import quotes.reflect.*
        tree match
            case ClassDef(name, cstr, parents, self, body) =>
                val owner = tree.symbol
                val sym = Symbol.newMethod(tree.symbol, "foo", ByNameType.apply(TypeRepr.of[String]))
                val mtd = DefDef.apply(sym, _ => Some(Literal(StringConstant("Hello, I was added by a MacroAnnotation and without being defined in the class."))))
                List(ClassDef.copy(tree)(name, cstr, parents, self, mtd :: body))
            case _ => report.errorAndAbort(s"@implementAFoo can only be applied to classes that extend AFoo")

end implementAFoo
