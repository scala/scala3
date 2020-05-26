object Test {
  inline def s1 = 47 *: s2

  inline def s2 = 48 *: s3

  inline def s3 = 49 *: Tuple()

  s1
}
