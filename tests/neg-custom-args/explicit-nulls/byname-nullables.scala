object Test1 with

  def f(x: String) =
    x ++ x

  def g() =
    var x: String | Null = "abc"
    if x != null then f(x)   // OK: f is call-by-value
    else x


object Test2 with

  def f(x: => String) =
    x ++ x

  def g() =
    var x: String | Null = "abc"
    if x != null then f(x)   // error: f is call-by-name
    else x

object Test3 with

  def f(x: String, y: String) = x

  def f(x: => String | Null, y: Int) =
    x

  def g() =
    var x: String | Null = "abc"
    if x != null then f(x, 1)   // OK: not-null check successfully dropped
    else x

object Test4 with

  def f(x: String, y: String) = x

  def f(x: => String | Null, y: Int) =
    x

  def g() =
    var x: String | Null = "abc"
    if x != null then f(identity(x), 1)   // error: dropping not null check fails typing
    else x

object Test5 with
  import compiletime.byName

  def f(x: String, y: String) = x

  def f(x: => String | Null, y: Int) =
    x

  def g() =
    var x: String | Null = "abc"
    if x != null then f(byName(identity(x)), 1)   // OK, byName avoids the flow typing
    else x

object Test6 with

  def f(x: String, y: String) = x

  def f(x: => String, y: Int) =
    x

  def g() =
    var x: String | Null = "abc"
    if x != null then f(x, 1)   // error: dropping not null check typechecks OK, but gives incompatible result type
    else x

object Test7 with
  import compiletime.byName

  def f(x: String, y: String) = x

  def f(x: => String, y: Int) =
    x

  def g() =
    var x: String | Null = "abc"
    if x != null then f(byName(x), 1)   // error: none of the overloaded methods match argument types
    else x
