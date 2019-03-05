package scala.tasty
package reflect

trait ConstantOps extends Core {

  implicit class ConstantAPI(const: Constant) {
    def value: Any = kernel.Constant_value(const)
  }

  /** Module of Constant literals */
  object Constant {

    /** Module of Null literals */
    object Unit {
      /** Unit `()` literal */
      def apply(): Constant =
        kernel.Constant_Unit_apply()

      /** Extractor for Unit literals */
      def unapply(constant: Constant): Boolean =
        kernel.matchConstant_Unit(constant)
    }

    /** Module of Null literals */
    object Null {
      /** `null` literal */
      def apply(): Constant =
        kernel.Constant_Null_apply()

      /** Extractor for Null literals */
      def unapply(constant: Constant): Boolean =
        kernel.matchConstant_Null(constant)
    }

    /** Module of Boolean literals */
    object Boolean {
      /** Boolean literal */
      def apply(x: Boolean): Constant =
        kernel.Constant_Boolean_apply(x)

      /** Extractor for Boolean literals */
      def unapply(constant: Constant): Option[Boolean] =
        kernel.matchConstant_Boolean(constant)
    }

    /** Module of Byte literals */
    object Byte {
      /** Byte literal */
      def apply(x: Byte): Constant =
        kernel.Constant_Byte_apply(x)

      /** Extractor for Byte literals */
      def unapply(constant: Constant): Option[Byte] =
        kernel.matchConstant_Byte(constant)
    }

    /** Module of Short literals */
    object Short {
      /** Short literal */
      def apply(x: Short): Constant =
        kernel.Constant_Short_apply(x)

      /** Extractor for Short literals */
      def unapply(constant: Constant): Option[Short] =
        kernel.matchConstant_Short(constant)
    }

    /** Module of Char literals */
    object Char {
      /** Char literal */
      def apply(x: Char): Constant =
        kernel.Constant_Char_apply(x)

      /** Extractor for Char literals */
      def unapply(constant: Constant): Option[Char] =
        kernel.matchConstant_Char(constant)
    }

    /** Module of Int literals */
    object Int {
      /** Int literal */
      def apply(x: Int): Constant =
        kernel.Constant_Int_apply(x)

      /** Extractor for Int literals */
      def unapply(constant: Constant): Option[Int] =
        kernel.matchConstant_Int(constant)
    }

    /** Module of Long literals */
    object Long {
      /** Long literal */
      def apply(x: Long): Constant =
        kernel.Constant_Long_apply(x)

      /** Extractor for Long literals */
      def unapply(constant: Constant): Option[Long] =
        kernel.matchConstant_Long(constant)
    }

    /** Module of Float literals */
    object Float {
      /** Float literal */
      def apply(x: Float): Constant =
        kernel.Constant_Float_apply(x)

      /** Extractor for Float literals */
      def unapply(constant: Constant): Option[Float] =
        kernel.matchConstant_Float(constant)
    }

    /** Module of Double literals */
    object Double {
      /** Double literal */
      def apply(x: Double): Constant =
        kernel.Constant_Double_apply(x)

      /** Extractor for Double literals */
      def unapply(constant: Constant): Option[Double] =
        kernel.matchConstant_Double(constant)
    }

    /** Module of String literals */
    object String {
      /** String literal */
      def apply(x: String): Constant =
        kernel.Constant_String_apply(x)

      /** Extractor for String literals */
      def unapply(constant: Constant): Option[String] =
        kernel.matchConstant_String(constant)
    }

    /** Module of ClassTag literals */
    object ClassTag {
      /** scala.reflect.ClassTag literal */
      def apply[T](implicit x: scala.reflect.ClassTag[T]): Constant =
        kernel.Constant_ClassTag_apply(x)

      /** Extractor for ClassTag literals */
      def unapply(constant: Constant): Option[Type] =
        kernel.matchConstant_ClassTag(constant)
    }

    /** Module of scala.Symbol literals */
    object Symbol {
      /** scala.Symbol literal */
      def apply(x: scala.Symbol): Constant =
        kernel.Constant_Symbol_apply(x)

      /** Extractor for scala.Symbol literals */
      def unapply(constant: Constant): Option[scala.Symbol] =
        kernel.matchConstant_Symbol(constant)
    }
  }
}
