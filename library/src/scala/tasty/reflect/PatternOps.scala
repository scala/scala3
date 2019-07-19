package scala.tasty
package reflect

trait PatternOps extends Core {

  implicit class ValueAPI(value: Value) {
    def value given (ctx: Context): Term = kernel.Pattern_Value_value(value)
  }

  implicit class BindAPI(bind: Bind) {
    def name given (ctx: Context): String = kernel.Pattern_Bind_name(bind)
    def pattern given (ctx: Context): Pattern = kernel.Pattern_Bind_pattern(bind)
  }

  implicit class UnapplyAPI(unapply: Unapply) {
    def fun given (ctx: Context): Term = kernel.Pattern_Unapply_fun(unapply)
    def implicits given (ctx: Context): List[Term] = kernel.Pattern_Unapply_implicits(unapply)
    def patterns given (ctx: Context): List[Pattern] = kernel.Pattern_Unapply_patterns(unapply)
  }

  implicit class AlternativesAPI(alternatives: Alternatives) {
    def patterns given (ctx: Context): List[Pattern] = kernel.Pattern_Alternatives_patterns(alternatives)
  }

  implicit class TypeTestAPI(typeTest: TypeTest) {
    def tpt given (ctx: Context): TypeTree = kernel.Pattern_TypeTest_tpt(typeTest)
  }

  implicit class PatternAPI(self: Pattern) {
    /** Position in the source code */
    def pos given (ctx: Context): Position = kernel.Pattern_pos(self)

    def tpe given (ctx: Context): Type = kernel.Pattern_tpe(self)

    def symbol given (ctx: Context): Symbol = kernel.Pattern_symbol(self)
  }

  object Pattern {

    object IsValue {
      def unapply(pattern: Pattern) given (ctx: Context): Option[Value] =
        kernel.matchPattern_Value(pattern)
    }

    object Value {
      def apply(tpt: Term) given (ctx: Context): Value =
        kernel.Pattern_Value_module_apply(tpt)
      def copy(original: Value)(tpt: Term) given (ctx: Context): Value =
        kernel.Pattern_Value_module_copy(original)(tpt)
      def unapply(pattern: Pattern) given (ctx: Context): Option[Term] =
        kernel.matchPattern_Value(pattern).map(_.value)
    }

    object IsBind {
      def unapply(pattern: Pattern) given (ctx: Context): Option[Bind] =
        kernel.matchPattern_Bind(pattern)
    }

    object Bind {
      // TODO def apply(name: String, pattern: Pattern) given (ctx: Context): Bind
      def copy(original: Bind)(name: String, pattern: Pattern) given (ctx: Context): Bind =
        kernel.Pattern_Bind_module_copy(original)(name, pattern)
      def unapply(pattern: Pattern) given (ctx: Context): Option[(String, Pattern)] =
        kernel.matchPattern_Bind(pattern).map(x => (x.name, x.pattern))
    }

    object IsUnapply {
      def unapply(pattern: Pattern) given (ctx: Context): Option[Unapply] =
        kernel.matchPattern_Unapply(pattern)
    }

    object Unapply {
      // TODO def apply(fun: Term, implicits: List[Term], patterns: List[Pattern]) given (ctx: Context): Unapply
      def copy(original: Unapply)(fun: Term, implicits: List[Term], patterns: List[Pattern]) given (ctx: Context): Unapply =
        kernel.Pattern_Unapply_module_copy(original)(fun, implicits, patterns)
      def unapply(pattern: Pattern) given (ctx: Context): Option[(Term, List[Term], List[Pattern])] =
        kernel.matchPattern_Unapply(pattern).map(x => (x.fun, x.implicits, x.patterns))
    }

    object IsAlternatives {
      def unapply(pattern: Pattern) given (ctx: Context): Option[Alternatives] =
        kernel.matchPattern_Alternatives(pattern)
    }

    object Alternatives {
      def apply(patterns: List[Pattern]) given (ctx: Context): Alternatives =
        kernel.Pattern_Alternatives_module_apply(patterns)
      def copy(original: Alternatives)(patterns: List[Pattern]) given (ctx: Context): Alternatives =
        kernel.Pattern_Alternatives_module_copy(original)(patterns)
      def unapply(pattern: Pattern) given (ctx: Context): Option[List[Pattern]] =
        kernel.matchPattern_Alternatives(pattern).map(_.patterns)
    }

    object IsTypeTest {
      def unapply(pattern: Pattern) given (ctx: Context): Option[TypeTest] =
        kernel.matchPattern_TypeTest(pattern)
    }

    object TypeTest {
      def apply(tpt: TypeTree) given (ctx: Context): TypeTest =
        kernel.Pattern_TypeTest_module_apply(tpt)
      def copy(original: TypeTest)(tpt: TypeTree) given (ctx: Context): TypeTest =
        kernel.Pattern_TypeTest_module_copy(original)(tpt)
      def unapply(pattern: Pattern) given (ctx: Context): Option[TypeTree] =
        kernel.matchPattern_TypeTest(pattern).map(_.tpt)
    }

    object IsWildcardPattern {
      def unapply(pattern: Pattern) given (ctx: Context): Option[WildcardPattern] =
        kernel.matchPattern_WildcardPattern(pattern)
    }

    object WildcardPattern {
      def apply(tpe: TypeOrBounds) given (ctx: Context): WildcardPattern =
        kernel.Pattern_WildcardPattern_module_apply(tpe)
      def unapply(pattern: Pattern) given (ctx: Context): Boolean =
        kernel.matchPattern_WildcardPattern(pattern).isDefined
    }

  }

}
