package scala.tasty
package reflect

trait PatternOps extends Core {

  implicit def ValueDeco(value: Value): Pattern.ValueAPI
  implicit def BindDeco(bind: Bind): Pattern.BindAPI
  implicit def UnapplyDeco(unapply: Unapply): Pattern.UnapplyAPI
  implicit def AlternativeDeco(alternatives: Alternatives): Pattern.AlternativesAPI
  implicit def TypeTestDeco(typeTest: TypeTest): Pattern.TypeTestAPI

  trait PatternAPI {
    /** Position in the source code */
    def pos(implicit ctx: Context): Position

    def tpe(implicit ctx: Context): Type

    def symbol(implicit ctx: Context): Symbol
  }
  implicit def PatternDeco(pattern: Pattern): PatternAPI

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

    trait ValueAPI {
      def value(implicit ctx: Context): Term
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

    trait BindAPI {
      def name(implicit ctx: Context): String
      def pattern(implicit ctx: Context): Pattern
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

    trait UnapplyAPI {
      def fun(implicit ctx: Context): Term
      def implicits(implicit ctx: Context): List[Term]
      def patterns(implicit ctx: Context): List[Pattern]
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

    trait AlternativesAPI {
      def patterns(implicit ctx: Context): List[Pattern]
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

    trait TypeTestAPI {
      def tpt(implicit ctx: Context): TypeTree
    }

  }

}
