//> using options -source:3.9
/* vals */
val goodVal = 1
val $startVal = 1 // warn
val mid$dleVal = 1 // warn
val endVal$ = 1 // warn

def testValUsage =
  $startVal + endVal$ // ok, should only warn on declaration not on usage

/* functions */
def $funcStart() = 3 // warn
def func$Middle() = 3 // warn
def funcEnd$() = 3 // warn

def func1badArg(goodArg: Int, bad$Arg: Int) = 5 // warn
def func1badArgAndUsageDoesNotThrowWarning(goodArg: Int, bad$Arg: Int) = bad$Arg // warn
def func2badArgs(goodArg: Int, bad$Arg: Int, bad$arg2: String) = 5 // warn // warn
def multilineFunc(
  goodArg: Int,
  badAr$g: Int // warn
) = 1

def testFuncUsaage =
  $funcStart() + func1badArg(1, 2) // ok, should only warn on declaration not on usage

/* types */
type GoodType = Int
type $StartType = Int // warn
type Middle$Type = Int // warn
type EndType$ = Int // warn

val typedVal: Middle$Type = 2 // ok, should only warn on declaration not on usage
def funcWithDollarTypes(foo: $StartType): Middle$Type = 2 // ok, should only warn on declaration not on usage

/* enums */
enum GoodEnum:
  case GoodCase
  case $BadCaseStart // warn
  case BadCase$Middle // warn
  case BadCaseEnd$ // warn

enum $BadEnumStart: // warn
  case GoodCase
  case $BadCase // warn

enum BadEnum$Middle: // warn
  case GoodCase
  case Bad$Case // warn

enum BadEnumEnd$: // warn
  case GoodCase
  case BadCase$ // warn

enum E$numWithEndKeyword: // warn
  case SomeCase
end E$numWithEndKeyword // ok

def TestEnumUsage(a: $BadEnumStart): Int = // ok, should only warn on declaration not on usage
  a match
    case $BadEnumStart.GoodCase => 1
    case $BadEnumStart.$BadCase => 2 // ok, should only warn on declaration not on usage

/* objects */
object $ObjectStart: // warn
  val goodVal = 1
  val $badVal = 2 // warn

object Object$Middle: // warn
  val goodVal = 1
  val bad$Val = 2 // warn

object ObjectEnd$: // warn
  val goodVal = 1
  val badVal$ = 2 // warn

object GoodObject:
  val goodVal = 1
  val b$adVal = 2 // warn

object Ob$jectWithEndKeyword: // warn
  val someVal = 1
end Ob$jectWithEndKeyword // ok

val testObjectUsage = ObjectEnd$.badVal$ // ok, should only warn on declaration not on usage

/* case classes */
case class $InlineCaseClassStart(someField: Int) // warn
case class InlineCaseClass$Middle(someField: Int) // warn
case class InlineCaseClassEnd$(someField: Int) // warn

case class InlineCaseClass(goodField: Int, badFiel$d: Int) // warn

case class $CaseClassStart( // warn
  somefield: Int,
  b$adfield: Int // warn
)

case class CaseClass$Middle( // warn
  somefield: Int,
  bad$Field: Int // warn
)

case class CaseClassEnd$( // warn
  somefield: Int,
  badField$: Int // warn
)

// companion oject
object CaseClassEnd$: // warn
  val food = 1

val testCaseClassUsage = CaseClass$Middle(somefield = 1, bad$Field = 2) // ok, should only warn on declaration not on usage

/* classes */
class GoodClass
class $StartClass // warn
class Middle$Class // warn
class EndClass$ // warn

class Cla$$( // warn
   var goodMember: Int,
   var badM$ember: Int // warn
):
  def goodMethod(x: Int) = badM$ember // ok, only checking if the method name does not contain a dollar sign
  def bad$Method(y: Int) = goodMember // warn
  def methodWithBadArgNames(b$ad$arg: Int) = goodMember // warn
  def method$WithEndKeyword() = // warn
    3
  end method$WithEndKeyword
end Cla$$ // ok

def testUsage =
  val instantiation = new Cla$$(goodMember = 1, badM$ember = 2) // ok, should only warn on declaration not on usage
  instantiation.bad$Method(1) // ok, should only warn on declaration not on usage
  instantiation.methodWithBadArgNames(2) // ok, should only warn on declaration not on usage


/* traits */
trait GoodTrait
trait $BadTraitStart // warn
trait BadTrait$Middle // warn
trait BadTraitEnd$ // warn

class TestTraitUsage extends $BadTraitStart // ok, should only warn on declaration not on usage

package GoodPackage:
  val goodVal = 1
  val b$adVal = 2 // warn

package $BadPackageStart: // warn
  val goodVal = 1
  val $badVal = 2 // warn

package BadPackage$Middle: // warn
  val goodVal = 1
  val bad$Val = 2 // warn

package BadPackageEnd$ : // warn
  val goodVal = 1
  val badVal$ = 2 // warn

def patvar[A](x: Option[A]) =
  x match
  case Some(funky$thing) => true // warn
  case _ => false
