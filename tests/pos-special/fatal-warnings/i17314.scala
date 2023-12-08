//> using options "-Wunused:all"

import java.net.URI

object circelike {
  import scala.compiletime.summonInline
  import scala.deriving.Mirror

  type Codec[T]
  type Configuration
  trait ConfiguredCodec[T]
  object ConfiguredCodec:
    inline final def derived[A](using conf: Configuration)(using
      inline mirror: Mirror.Of[A]
    ): ConfiguredCodec[A] =
      new ConfiguredCodec[A]:
        val codec = summonInline[Codec[URI]] // simplification
}

object foo {
  import circelike.{Codec, Configuration}

  given Configuration = ???
  given Codec[URI] = ???
}

object bar {
  import circelike.Codec
  import circelike.{Configuration, ConfiguredCodec}
  import foo.{given Configuration, given Codec[URI]}

  case class Operator(url: URI) derives ConfiguredCodec
}
