class Input(x: String)

trait Decoder[T]

object Decoder {
  inline def apply[T](implicit f: () => Unit): Decoder[T] = ???
}

object Input {
  given Decoder[Input] = Decoder { () =>
    Input("")
  }
}
