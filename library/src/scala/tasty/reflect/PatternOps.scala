package scala.tasty
package reflect

trait PatternOps extends Core {

  trait PatternAPI {
    /** Position in the source code */
    def pos(implicit ctx: Context): Position

    def tpe(implicit ctx: Context): Type
  }
  implicit def PatternDeco(pattern: Pattern): PatternAPI

  val Pattern: PatternModule
  abstract class PatternModule {

    val Value: ValueModule
    abstract class ValueModule {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[Term]
    }

    val Bind: BindModule
    abstract class BindModule {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[(String, Pattern)]
    }

    val Unapply: UnapplyModule
    abstract class UnapplyModule {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[(Term, List[Term], List[Pattern])]
    }

    val Alternative: AlternativeModule
    abstract class AlternativeModule {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[List[Pattern]]
    }

    val TypeTest: TypeTestModule
    abstract class TypeTestModule {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[TypeTree]
    }
  }

}
