package tests
package traitmethodsgrouping

trait Promise:
  def shouldBeAbstract: Int
  def shouldBeConcrete: Int = 1
