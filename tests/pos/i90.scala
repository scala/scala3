object Test {
  type F[X] = X match { case 10 => "0" case 11 => "1" }

  implicitly[F[Any] <:< String]
}
