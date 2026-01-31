//> using options -Werror

package test:
  final case class Pair(a: Int, b: Int)
  def x: Pair = ???

import test.*

package p:
  object Unpack {
    def unapply(e: Pair): NamedTuple.NamedTuple[("a", "b"), (Int, Int)] = ???

    x match {
      case Unpack(_, _) => ???
    }
  }

package q:
  object Unpack {
    def unapply(e: Pair): (a: Int, b: Int) = ???

    x match {
      case Unpack(_, _) => ???
    }
  }

package r:
  object Unpack {
    def unapply(e: Pair): Some[(a: Int, b: Int)] = ???

    x match {
      case Unpack(_, _) => ???
    }
  }

package s:
  object Unpack {
    def unapply(e: Pair): NamedTuple.NamedTuple[("v", "w"), (Int, Int)] = ???

    x match {
      case Unpack(_, _) => ???
    }
  }
