package exports.example

trait Decoder/*<-exports::example::Decoder#*/[+T/*<-exports::example::Decoder#[T]*/] {
  def decode/*<-exports::example::Decoder#decode().*/(a/*<-exports::example::Decoder#decode().(a)*/: Array/*->scala::Array#*/[Byte/*->scala::Byte#*/]): T/*->exports::example::Decoder#[T]*/
}

trait Encoder/*<-exports::example::Encoder#*/[-T/*<-exports::example::Encoder#[T]*/] {
  def encode/*<-exports::example::Encoder#encode().*/(t/*<-exports::example::Encoder#encode().(t)*/: T/*->exports::example::Encoder#[T]*/): Array/*->scala::Array#*/[Byte/*->scala::Byte#*/]
}

trait Codec/*<-exports::example::Codec#*/[T/*<-exports::example::Codec#[T]*/](decode/*<-exports::example::Codec#decode.*/: Decoder/*->exports::example::Decoder#*/[T/*->exports::example::Codec#[T]*/], encode/*<-exports::example::Codec#encode.*/: Encoder/*->exports::example::Encoder#*/[T/*->exports::example::Codec#[T]*/])
  extends Decoder/*->exports::example::Decoder#*/[T/*->exports::example::Codec#[T]*/] with Encoder/*->exports::example::Encoder#*/[T/*->exports::example::Codec#[T]*/] {
  export decode/*->exports::example::Codec#decode.*/._
  export encode/*->exports::example::Codec#encode.*/._
}
