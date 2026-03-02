// https://github.com/lampepfl/scala/scala3/15913

class injector[F]

object injectorFactory {
  def apply[F](overrides: String*): injector[F] = new injector[F]

  def apply[F](
    bootstrapActivation: Int = ???,
    overrides: Seq[String] = Seq.empty,
  ): injector[F] = new injector[F]
}

object Test extends App {
  println(
    injectorFactory[String](
      bootstrapActivation = 0
    )
  )
}
