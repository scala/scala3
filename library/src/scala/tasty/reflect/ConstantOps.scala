package scala.tasty
package reflect

trait ConstantOps extends Core {

  implicit class ConstantAPI(const: Constant) {
    def value: Any = kernel.Constant_value(const)
  }

  /** Module of Constant literals */
  object Constant {

    def apply(x: Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type): Constant =
      kernel.Constant_apply(x)

    def unapply(constant: Constant): Option[Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type] =
      kernel.matchConstant(constant)

    /** Module of ClassTag literals */
    object ClassTag {
      /** scala.reflect.ClassTag literal */
      def apply[T](implicit x: Type): Constant =
        kernel.Constant_ClassTag_apply(x)

      /** Extractor for ClassTag literals */
      def unapply(constant: Constant): Option[Type] =
        kernel.matchConstant_ClassTag(constant)
    }
  }
}
