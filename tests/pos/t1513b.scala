import language.experimental.namedTypeArguments
object Test {
  def f[
    T1 <: String,
    T2 <: Int,
    T3 <: Boolean
  ](a1: T1, a2: T2, a3: T3) = ()

  f                                          ("", 1, true)
  f[T1 = String]                             ("", 1, true)
  f[T2 = Int]                                ("", 1, true)
  f[T3 = Boolean]                            ("", 1, true)
  f[T1 = String,  T2 = Int]                  ("", 1, true)
  f[T1 = String,  T3 = Boolean]              ("", 1, true)
  f[T2 = Int,     T1 = String]               ("", 1, true)
  f[T2 = Int,     T3 = Boolean]              ("", 1, true)
  f[T3 = Boolean, T2 = Int]                  ("", 1, true)
  f[T3 = Boolean, T1 = String]               ("", 1, true)
  f[T1 = String,  T2 = Int,     T3 = Boolean]("", 1, true)
  f[T1 = String,  T3 = Boolean, T2 = Int]    ("", 1, true)
  f[T2 = Int,     T1 = String,  T3 = Boolean]("", 1, true)
  f[T2 = Int,     T3 = Boolean, T1 = String] ("", 1, true)
  f[T3 = Boolean, T1 = String,  T2 = Int]    ("", 1, true)
  f[T3 = Boolean, T2 = Int,     T1 = String] ("", 1, true)
  f[String,       Int,          Boolean]     ("", 1, true)
}
