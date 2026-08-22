trait Settings:
  inline def nestedInline = false
  inline def switch: Boolean = !nestedInline

trait Inner:
  def switch: Boolean
  def go: String = if switch then "Yes" else "No"

object Outer extends Inner, Settings

@main def Test() =
    println(Outer.go)
