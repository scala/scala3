trait Settings:
  inline def switch: Boolean = true

trait Inner:
  def switch: Boolean
  def go: String = if switch then "Yes" else "No"
