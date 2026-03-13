import scala.util.*, boundary.break

/** boundary/break as a replacement for non-local returns */
def indexOf[T](xs: List[T], elem: T): Int =
  boundary:
    for (x, i) <- xs.zipWithIndex do
      if x == elem then break(i)
    -1

def breakTest() =
  println("breakTest")
  assert(indexOf(List(1, 2, 3), 2) == 1)
  assert(indexOf(List(1, 2, 3), 0) == -1)

/** traverse becomes trivial to write */
def traverse[T](xs: List[Option[T]]): Option[List[T]] =
  optional(xs.map(_.?))

def optTest() =
  println("optTest")
  assert(traverse(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
  assert(traverse(List(Some(1), None, Some(3))) == None)

/** A check function returning a Result[Unit, _] */
inline def check[E](p: Boolean, err: E): Result[Unit, E] =
  if p then Ok(()) else Err(err)

/** Another variant of a check function that returns directly to the given
 *  label in case of error.
 */
inline def check_![E](p: Boolean, err: E)(using l: boundary.Label[Err[E]]): Unit =
  if p then () else break(Err(err))

/** Use `Result` to convert exceptions to `Err` values */
def parseDouble(s: String): Result[Double, Exception] =
  Result(s.toDouble)

def parseDoubles(ss: List[String]): Result[List[Double], Exception] =
  respond:
    ss.map(parseDouble(_).?)

/** Demonstrate combination of `check` and `.?`. */
def trySqrt(x: Double) = // inferred: Result[Double, String]
  respond:
    check(x >= 0, s"cannot take sqrt of negative $x").? // direct jump
    math.sqrt(x)

/** Instead of `check(...).?` one can also use `check_!(...)`.
 *  Note use of `mapErr` to convert Exception errors to String errors.
 */
def sumRoots(xs: List[String]) = // inferred: Result[Double, String]
  respond:
    check_!(xs.nonEmpty, "list is empty")           // direct jump
    val ys = parseDoubles(xs).mapErr(_.toString).?  // direct jump
    ys.reduce((x, y) => x + trySqrt(y).?)           // need exception to propagate `Err`

def resultTest() = {
  println("resultTest")
  def assertFail(value: Any, s: String) = value match
    case Err(msg: String) => assert(msg.contains(s))
  assert(sumRoots(List("1", "4", "9")) == Ok(6))
  assertFail(sumRoots(List("1", "-2", "4")), "cannot take sqrt of negative")
  assertFail(sumRoots(List()), "list is empty")
  assertFail(sumRoots(List("1", "3ab")), "NumberFormatException")

  val xs = sumRoots(List("1", "-2", "4")) *: sumRoots(List()) *: sumRoots(List("1", "3ab")) *: Result.empty
  xs match
    case Err(msgs) => assert(msgs.length == 3)
    case _ => assert(false)

  val ys = sumRoots(List("1", "2", "4")) *: sumRoots(List("1")) *: sumRoots(List("2")) *: Result.empty
  ys match
    case Ok((a, b, c)) => // ok
    case _ => assert(false)
}

def validateTest() = {
  import Validator.validate
  case class Form(name: String, age: Int)

  def validatedForm(name: String, age: Int, confirmed: Boolean): Result[Form, List[String]] =
    validate[String]: v =>
        v.ensure(!name.isEmpty, "Missing name", abort = true)
        v.ensure(name.head.isUpper, s"${name} does not start with uppercase letter")
        v.ensure(age >= 18, s"Age ${age} is below minimum age 18")
        v.ensure(confirmed, "Missing confirmation")
      .ifOK:
        Form(name, age)

  val p1 = validatedForm("Bob", 21, true)
  val p2 = validatedForm("bob", 21, true)
  val p3 = validatedForm("Bob", 16, true)
  val p4 = validatedForm("Bob", 21, false)
  val p5 = validatedForm("bob", 16, false)
  val p6 = validatedForm("", 16, false)
  println(p1)
  println(p2)
  println(p3)
  println(p4)
  println(p5)
  println(p6)
}

@main def Test =
  breakTest()
  optTest()
  resultTest()
  parseCsvIgnoreErrors()
  validateTest()