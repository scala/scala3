
trait Foo[A <: Foo[A]] {

  def next: A

  inline final def once: A = next

  def once1: A = once

  def twice: A = once.once
}

trait Bar extends Foo[Bar]

case object Bar0 extends Bar { def next = Bar1 }
case object Bar1 extends Bar { def next = Bar2 }
case object Bar2 extends Bar { def next = this }

object Test {

  def main(args: Array[String]): Unit = {
    assert(Bar0.once.once.toString == "Bar2")
    assert(Bar0.twice.toString == "Bar2")
  }
}

