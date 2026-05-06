package tests
package extendsCall

class Impl() extends Base(Seq.empty, c = "-") //expected: class Impl() extends Base

class Base(val a: Seq[String], val b: String = "", val c: String = "") //expected: class Base(val a: Seq[String], val b: String = ..., val c: String = ...)
