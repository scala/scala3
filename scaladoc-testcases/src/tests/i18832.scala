package tests
package i18832

trait Foo

trait Syntax:
  def mapTo = ()
  implicit def Foo(x: Unit): Syntax = this

val run = ().mapTo
