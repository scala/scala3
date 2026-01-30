//> using options -Werror

object A {
  @deprecated("this one is deprecated", "ever")
  def imdeprecated() = ()

  imdeprecated() // nopos-error
}
