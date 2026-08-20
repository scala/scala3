// --- Cases from #11253 ---

trait T extends Enum[T] { def t: String }

enum E1(val t: String) extends Enum[T], T { case X extends E1("xxx") } // error

// --- Cases from #11252 ---

enum E2 extends Enum[Nothing] { case X } // error

enum E3[A](val inner: A) extends Enum[E3[Int]] { // error
  case X extends E3[String]("hello")
}

// --- Java enum behavior: generic enums with wildcard type args rejected ---

enum E5[+A] extends Enum[E5[?]] { case X extends E5[Nothing] } // error

enum E6[A] extends Enum[E6[?]] { case X extends E6[Unit] } // error

// --- Cases from #9541 ---

trait U[E <: Enum[E]] extends Enum[E]

trait UBad extends U[UBad]

enum E7 extends UBad { case X } // error

// --- other cases ---

enum E[+T] extends java.lang.Enum[E[_]] { // error
  case S1, S2
  case C extends E[Int]
}

