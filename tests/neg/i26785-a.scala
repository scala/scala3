enum HList[+T]:
	case HNil extends HList[Nothing]
	case HCons[T0, E <: T0, L <: HList[T0]](h: E, t: L) extends HList[T0]

import HList.*

object Append:
	//                    v This variance causes typer to crash
	opaque type Append[T, +E <: T, L <: HList[T]] <: HList[T] = HList[T]
	def append[T, E <: T](lst: HList[T], elem: E): Append[T, E, lst.type] = ???
	transparent inline def apply[T, E <: T](lst: HList[T], elem: E): Append[T, E, lst.type] =
		append[T, E](lst, elem)

object Rev:
	opaque type Rev[T, L <: HList[T]] <: HList[T] = HList[T]
	def rev[T](lst: HList[T]): Rev[T, lst.type] = ???
	transparent inline def apply[T](lst: HList[T]): Rev[T, lst.type] =
		inline lst match
			case HNil => HNil
			case HCons(h, t) => // error
				Append(Rev.rev(t), h)

object TestMain:
	import Rev.Rev
	def failure[T, L <: HList[T]](lst: L): Nothing =
		lst match
			case base: HNil.type => ???
			case ind @ HCons(h, t) => // error
				val g = Rev(ind)
		???