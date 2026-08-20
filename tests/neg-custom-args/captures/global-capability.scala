package test

object Console: // error
  private val out: java.io.PrintStream^ = System.out
  def println(s: String) = out.println(s)

object Console2: // error
  private val out: java.io.PrintStream^ = System.out
  private val in: java.io.InputStream^ = System.in
