import scala.quoted._

trait Foo {
  protected def pSelect: Int = 4
  protected def pApply(i: Int): Int = 4 + i
  protected def pTypeApply[T]: T = ???
  def foo: Int
}

// base case
inline def baseCaseMacro: Foo = ${baseCaseMacroImpl()}
def baseCaseMacroImpl(using Quotes)(): Expr[Foo] =
  '{
    new Foo {
      def foo: Int = pSelect
    }
  }

// base case - transparent
transparent inline def baseCaseTransparentMacro: Foo = ${baseCaseTransparentMacroImpl()}
def baseCaseTransparentMacroImpl(using Quotes)(): Expr[Foo] =
  '{
    new Foo {
      def foo: Int = pSelect
    }
  }

// nested classes
inline def nestedClassesMacro: Foo = ${nestedClassesMacroImpl()}
def nestedClassesMacroImpl(using Quotes)(): Expr[Foo] =
  '{
    new Foo { a =>
      def foo: Int = 0
      def bar =
        new Foo { b =>
          def foo: Int = a.pSelect + pSelect + b.pSelect
        }
    }
  }

// apply
inline def applyMacro: Foo = ${applyMacroImpl()}
def applyMacroImpl(using Quotes)(): Expr[Foo] =
  '{
    new Foo {
      def foo: Int = pApply(1)
    }
  }

// type apply
inline def typeApplyMacro: Foo = ${typeApplyMacroImpl()}
def typeApplyMacroImpl(using Quotes)(): Expr[Foo] =
  '{
    new Foo {
      def foo: Int = pTypeApply[Int]
    }
  }
