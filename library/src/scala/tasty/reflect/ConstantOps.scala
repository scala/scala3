package scala.tasty
package reflect

trait ConstantOps extends Core {

  given (const: Constant) {
    def value: Any = internal.Constant_value(const)
  }

  /** Module of Constant literals */
  object Constant {

    def apply(x: Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type): Constant =
      internal.Constant_apply(x)

    def unapply(constant: Constant): Option[Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type] =
      internal.matchConstant(constant)

    /** Module of ClassTag literals */
    object ClassTag {
      /** scala.reflect.ClassTag literal */
      def apply[T](given x: Type): Constant =
        internal.Constant_ClassTag_apply(x)

      /** Extractor for ClassTag literals */
      def unapply(constant: Constant): Option[Type] =
        internal.matchConstant_ClassTag(constant)
    }
  }
}
