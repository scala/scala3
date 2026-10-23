// A facade over `List` whose method signatures encode size facts, so that a
// checked `zip` verifies statically with no `runtimeChecked` in its body.
//
// `SList`'s underlying `List` is only transparent inside `SLists`. Every
// signature that mentions `.size` therefore lives *outside* `SLists`, so that
// `.size` always resolves to the facade extension below, never to the colliding
// `List.size` that is visible on the transparent underlying.

object SLists:
  opaque type SList[A] = List[A]
  def wrap[A](l: List[A]): SList[A] = l
  def unwrap[A](s: SList[A]): List[A] = s


val pairs = List(1, 2).zip(List("a"))
// List((1, "a"))

import SLists.*

object SList:
  def empty[A]: {r: SList[A] with r.size == 0} =
    wrap(Nil: List[A]).runtimeChecked
  def cons[A](x: A, xs: SList[A]): {r: SList[A] with r.size == xs.size + 1} =
    wrap(x :: unwrap(xs)).runtimeChecked

extension [A](a: SList[A])
  def size: {r: Int with r >= 0} = unwrap(a).size.runtimeChecked

extension [A](a: SList[A] with a.size > 0)
  def head: A = unwrap(a).head
  def tail: {r: SList[A] with r.size == a.size - 1} =
    wrap(unwrap(a).tail).runtimeChecked

def zip[A, B](
  xs: SList[A],
  ys: SList[B] with ys.size == xs.size
): {res: SList[(A, B)] with res.size == xs.size} =
  if xs.size == 0 then
    SList.empty[(A, B)]
  else if xs.size > 0 then
    SList.cons((xs.head, ys.head), zip(xs.tail, ys.tail))
  else throw AssertionError("unreachable: size is non-negative")
