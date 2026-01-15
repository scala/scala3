class C:

  //final val Seconds = java.util.concurrent.TimeUnit.SECONDS
  //inline val Seconds = java.util.concurrent.TimeUnit.SECONDS // not a literal
  inline transparent def Seconds = java.util.concurrent.TimeUnit.SECONDS

  @OutputTimeUnit(Seconds)
  def f(): Unit = ()

  @OutputTimeUnit(java.util.concurrent.TimeUnit.SECONDS)
  def ok(): Unit = ()
