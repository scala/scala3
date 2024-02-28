//> using options -experimental -Yno-experimental

import annotation.MacroAnnotation
import quoted.*

trait Foo

class extendFoo extends MacroAnnotation :
    override def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
        import quotes.reflect.*
        tree match
            case ClassDef(name, ctr, p, self, body) =>
                val parents = List(TypeTree.of[Foo])
                val newTree = ClassDef.copy(tree)(name, ctr,  parents, self, body)
                newTree :: Nil
            case _ =>
                report.error("@extendFoo can only annotate class definitions")
                tree :: Nil