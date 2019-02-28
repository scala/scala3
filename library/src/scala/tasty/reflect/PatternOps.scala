package scala.tasty
package reflect

trait PatternOps extends Core {

  implicit class ValueAPI(value: Value) {
    def value(implicit ctx: Context): Term = kernel.Pattern_Value_value(value)
  }

  implicit class BindAPI(bind: Bind) {
    def name(implicit ctx: Context): String = kernel.Pattern_Bind_name(bind)
    def pattern(implicit ctx: Context): Pattern = kernel.Pattern_Bind_pattern(bind)
  }

  implicit class UnapplyAPI(unapply: Unapply) {
    def fun(implicit ctx: Context): Term = kernel.Pattern_Unapply_fun(unapply)
    def implicits(implicit ctx: Context): List[Term] = kernel.Pattern_Unapply_implicits(unapply)
    def patterns(implicit ctx: Context): List[Pattern] = kernel.Pattern_Unapply_patterns(unapply)
  }

  implicit class AlternativesAPI(alternatives: Alternatives) {
    def patterns(implicit ctx: Context): List[Pattern] = kernel.Pattern_Alternatives_patterns(alternatives)
  }

  implicit class TypeTestAPI(typeTest: TypeTest) {
    def tpt(implicit ctx: Context): TypeTree = kernel.Pattern_TypeTest_tpt(typeTest)
  }

  implicit class PatternAPI(self: Pattern) {
    /** Position in the source code */
    def pos(implicit ctx: Context): Position = kernel.Pattern_pos(self)

    def tpe(implicit ctx: Context): Type = kernel.Pattern_tpe(self)

    def symbol(implicit ctx: Context): Symbol = kernel.Pattern_symbol(self)
  }

  val Pattern: PatternModule
  abstract class PatternModule {

    val IsValue: IsValueModule
    abstract class IsValueModule {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[Value]
    }

    val Value: ValueModule
    abstract class ValueModule {
      def apply(tpt: Term)(implicit ctx: Context): Value
      def copy(original: Value)(tpt: Term)(implicit ctx: Context): Value
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[Term]
    }

    val IsBind: IsBindModule
    abstract class IsBindModule {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[Bind]
    }

    val Bind: BindModule
    abstract class BindModule {
      // TODO def apply(name: String, pattern: Pattern)(implicit ctx: Context): Bind
      def copy(original: Bind)(name: String, pattern: Pattern)(implicit ctx: Context): Bind
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[(String, Pattern)]
    }

    val IsUnapply: IsUnapplyModule
    abstract class IsUnapplyModule {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[Unapply]
    }

    val Unapply: UnapplyModule
    abstract class UnapplyModule {
      // TODO def apply(fun: Term, implicits: List[Term], patterns: List[Pattern])(implicit ctx: Context): Unapply
      def copy(original: Unapply)(fun: Term, implicits: List[Term], patterns: List[Pattern])(implicit ctx: Context): Unapply
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[(Term, List[Term], List[Pattern])]
    }

    val IsAlternatives: IsAlternativesModule
    abstract class IsAlternativesModule {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[Alternatives]
    }

    val Alternatives: AlternativesModule
    abstract class AlternativesModule {
      def apply(patterns: List[Pattern])(implicit ctx: Context): Alternatives
      def copy(original: Alternatives)(patterns: List[Pattern])(implicit ctx: Context): Alternatives
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[List[Pattern]]
    }

    val IsTypeTest: IsTypeTestModule
    abstract class IsTypeTestModule {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[TypeTest]
    }

    val TypeTest: TypeTestModule
    abstract class TypeTestModule {
      def apply(tpt: TypeTree)(implicit ctx: Context): TypeTest
      def copy(original: TypeTest)(tpt: TypeTree)(implicit ctx: Context): TypeTest
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[TypeTree]
    }

  }

}
