package test

object Console extends caps.ExclusiveCapability:
  private val out: java.io.PrintStream^ = System.out
  def println(s: String) = out.println(s)

