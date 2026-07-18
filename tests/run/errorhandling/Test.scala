import scala.util.*, boundary.break

import caps.any

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
  println("validateTest")
  import Validation.validate

  case class Email private (value: String)
  object Email:
    def from(raw: String): Result[Email, String] =
      if raw.contains("@") then Ok(Email(raw)) // demo!!
      else Err(s"Invalid email ${raw}")
  case class Form(name: String, email: Email, age: Int)

  def validatedForm(name: String, rawEmail: String, age: Int, confirmed: Boolean): Result[Form, List[String]] =
    validate: v =>
      v.require(!name.isEmpty, "Missing name")
      v.test(name.head.isUpper, s"${name} does not start with uppercase letter")
      val email = v.test(Email.from(rawEmail))
      v.test(age >= 18, s"Age ${age} is below minimum age 18")
      v.test(confirmed, "Missing confirmation")
      Form(name, email.valid, age)

  // Good stuff: can no longer edit the scope if you leak it
  // def leak(): Unit =
  //   val leaked: Result[Validation[String]^, List[String]] = Validation.validate: v =>
  //     v
  //   leaked match
  //     case Ok(scope) =>
  //       scope.appendOne("leaked error") // error
  //       ???
  //     case Err(errs) => ()

  type JsonDict = Map[String, String]
  def jsonDict(elems: (String, String)*): JsonDict = Map(elems*)

  enum InvoiceOrRefund:
    case Invoice(customerId: String, amount: BigInt)
    case Refund(invoiceId: String, reason: String)

  def validateJson(json: JsonDict): Result[InvoiceOrRefund, List[String]] =
    validate: v =>
      val kind = v.require(Result.fromOption(json.get("kind"), "missing 'kind'"))
      v.require(kind == "invoice" || kind == "refund", s"invalid 'kind' ${kind}")
      if kind == "invoice" then
        val customerId = v.test(Result.fromOption(json.get("customerId"), s"Missing customerId"))
        val amount = v.test {
          respond:
            val amount = Result.fromOption(json.get("amount"), s"Missing amount").?
            Result(BigInt(amount)).mapErr(err => s"Invalid amount: ${err.getMessage}").?
        }
        InvoiceOrRefund.Invoice(customerId.valid, amount.valid)
      else
        val invoiceId = v.test(Result.fromOption(json.get("invoiceId"), s"Missing invoiceId"))
        val reason = v.test(Result.fromOption(json.get("reason"), s"Missing reason"))
        InvoiceOrRefund.Refund(invoiceId.valid, reason.valid)

  def printResult[T, E](result: Result[T, List[E]]): Unit = result match
    case Ok(value) => println(s"ok: $value")
    case Err(errs) => println(s"err: ${errs.zipWithIndex.map { case (e, i) => s"[$i]: $e" }.mkString(", ")}")

  val p1 = validatedForm("Bob", "Bob@example.com", 21, true)  // TTTT
  val p2 = validatedForm("", "Bob@example.com", 21, true)     // F (abort early)
  val p3 = validatedForm("bob", "Bob@example.com", 21, true)  // FTTT
  val p4 = validatedForm("Bob", "bad-email", 21, true)        // TFTT
  val p5 = validatedForm("Bob", "Bob@example.com", 16, true)  // TTFT
  val p6 = validatedForm("Bob", "Bob@example.com", 21, false) // TTTF
  val p7 = validatedForm("bob", "bad-email", 16, false)       // FFFF
  printResult(p1)
  printResult(p2)
  printResult(p3)
  printResult(p4)
  printResult(p5)
  printResult(p6)
  printResult(p7)

  println("----")

  val j1 = validateJson(jsonDict("kind" -> "invoice", "customerId" -> "c1", "amount" -> "100")) // ok
  val j2 = validateJson(jsonDict("kind" -> "refund", "invoiceId" -> "i1", "reason" -> "bad product")) // ok
  val j3 = validateJson(jsonDict("kind" -> "invoice")) // [0]: missing customerId, [1]: missing amount
  val j4 = validateJson(jsonDict("kind" -> "invoice", "customerId" -> "c1", "amount" -> "bad amount")) // bad amount
  val j5 = validateJson(jsonDict("kind" -> "refund")) // [0]: missing invoiceId, [1]: missing reason
  val j6 = validateJson(jsonDict("kind" -> "other")) // invalid kind
  val j7= validateJson(jsonDict()) // missing kind
  printResult(j1)
  printResult(j2)
  printResult(j3)
  printResult(j4)
  printResult(j5)
  printResult(j6)
  printResult(j7)
}

@main def Test =
  breakTest()
  optTest()
  resultTest()
  parseCsvIgnoreErrors()
  validateTest()
