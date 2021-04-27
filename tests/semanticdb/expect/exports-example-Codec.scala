package exports.example

trait Decoder[+T] {
  def decode(a: Array[Byte]): T
}

trait Encoder[-T] {
  def encode(t: T): Array[Byte]
}

trait Codec[T](decode: Decoder[T], encode: Encoder[T])
  extends Decoder[T] with Encoder[T] {
  export decode._
  export encode._
}
