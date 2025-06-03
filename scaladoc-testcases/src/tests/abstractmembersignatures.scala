package tests
package abstractmembersignatures


trait TestTrait:
  def shouldBeAbstract: Int
  def shouldBeConcrete: Int = 1

class TestClass:
  def shouldBeConcrete: Int = 1

abstract class TestInheritedAbstractMembers extends TestTrait

abstract class AbstractTestClass:
  def shouldBeAbstract: Int
  def shouldBeConcrete: Int = 1

object TestObject:
  abstract class AbstractInnerClass:
    def shouldBeAbstract: Int
    def shouldBeConcrete: Int = 1

  class InnerClass:
    def shouldBeConcrete: Int = 1
