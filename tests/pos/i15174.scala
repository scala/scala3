trait Error
sealed abstract class Codec[A] {
  type AvroType
  def encode(a: A): Either[Error, AvroType]
  // def decode(value: Any): Either[Error, A]
}

object Codec {
  type Aux[AvroType0, A] = Codec[A] {
    type AvroType = AvroType0
  }

  final def instance[AvroType0, A](
      encode: A => Either[Error, AvroType0],
      // decode: Any => Either[Error, A]
  ): Codec.Aux[AvroType0, A]  = ???

  implicit final def option[A](implicit codec: Codec[A]): Codec[Option[A]] = ???
  given Codec.Aux[Int, Int] = ???
}


@main def test() = {
  implicit val codec: Codec[Option[Int]] =
    Codec.instance(
      Codec.option[Int].encode
        // expands to:
        // {
        //   val a: Codec[Option[Int]] = Codec.option[Int](Codec.given_Aux_Int_Int)
        //   a.encode
        // },
      // Codec.option[Int].decode
    )
}
