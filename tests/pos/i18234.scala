//> using options -Xfatal-warnings -Wdollar-check

val goodVal = 1
def func() = 1
def func1(a: Int) = a
def func2(
  a: Int,
  b: Int
) = a + b

type GoodType = Int

val typedVal: GoodType = 2

enum GoodEnum:
  case GoodCase

case class CaseClass(someField: Int) // error

class GoodClass

class Class(
   var goodMember: Int,
):
  def goodMethod(x: Int) = goodMember
end Class

def testUsage =
  val instatiation = new Class(goodMember = 1)
  instatiation.goodMethod(1)


trait GoodTrait

class TestTraitUsage extends GoodTrait

package GoodPackage:
  val goodVal = 1