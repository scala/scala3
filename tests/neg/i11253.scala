// --- Cases from #11253 ---

trait T extends Enum[T] { def t: String }

enum E1(val t: String) extends Enum[T], T { case X extends E1("xxx") } // error

// --- Cases from #11252 ---

enum E2 extends Enum[Nothing] { case X } // error

enum E3[A](val inner: A) extends Enum[E3[Int]] { // error
  case X extends E3[String]("hello")
}

// --- Cases from #9541 ---

trait U[E <: Enum[E]] extends Enum[E]

trait UBad extends U[UBad]

enum E7 extends UBad { case X } // error

enum E8 extends U[E7] { case X } // error
