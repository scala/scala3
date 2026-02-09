class C:

  //inline val Seconds = java.util.concurrent.TimeUnit.SECONDS // not a literal
  inline transparent def Seconds = java.util.concurrent.TimeUnit.SECONDS

  @OutputTimeUnit(Seconds)
  def f(): Unit = ()

  final val SecondsFinalVal = java.util.concurrent.TimeUnit.SECONDS

  @OutputTimeUnit(SecondsFinalVal)
  def f2(): Unit = ()

  @OutputTimeUnit(java.util.concurrent.TimeUnit.SECONDS)
  def ok(): Unit = ()
