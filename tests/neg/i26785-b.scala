enum HList[+T]:
	case HCons[T0, E <: T0, L <: HList[T0]](t: L) extends HList[T0]

import HList.*

object Append:
	opaque type Append[T, L] <: HList[T] = HList[T]
	def append[T](lst: HList[T]): Append[T, lst.type] = ???
	transparent inline def apply[T](lst: HList[T]): Append[T, lst.type] =
		append[T](lst)

object Rev:
	opaque type Rev[T, L <: HList[T]] <: HList[T] = HList[T]
	def rev[T](lst: HList[T]): Rev[T, lst.type] = ???
	transparent inline def apply[T](inline lst2: HList[T]): Rev[T, lst2.type] = // error
		inline lst2 match
			case HCons(t) =>
				// This is the problematic line
				Append(Rev.rev(t))

object TestMain:
	import Rev.Rev
	def failure[T0, E <: T0, L <: HList[T0]](h: E, t: L): Unit =
		Rev(HCons(t))
