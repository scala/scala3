---
layout: doc-page
title: "Dropped: XML Literals"
movedTo: https://docs.scala-lang.org/scala3/reference/dropped-features/xml.html
---

XML Literals are still supported, but will be dropped in the near future, to
be replaced with [XML string interpolation](https://github.com/lampepfl/xml-interpolator):

```scala
import dotty.xml.interpolator.*

case class Person(name: String) { override def toString = name }

@main def test: Unit =
  val bill = Person("Bill")
  val john = Person("John")
  val mike = Person("Mike")
  val todoList = List(
    (bill, john, "Meeting", "Room 203, 11:00am"),
    (john, mike, "Holiday", "March 22-24")
  )
  // XML literals (to be dropped)
  val mails1 = for (from, to, heading, body) <- todoList yield
    <message>
      <from>{from}</from><to>{to}</to>
      <heading>{heading}</heading><body>{body}</body>
    </message>
  println(mails1)
  // XML string interpolation
  val mails2 = for (from, to, heading, body) <- todoList yield xml"""
    <message>
      <from>${from}</from><to>${to}</to>
      <heading>${heading}</heading><body>${body}</body>
    </message>"""
  println(mails2)
```

For more information, see the semester project [XML String Interpolator for Dotty](https://infoscience.epfl.ch/record/267527) by Yassin Kammoun (2019).
