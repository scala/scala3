//> using options -preview
// preview needed for into in 3.8

abstract enum Foo1 { case C } // error: only access modifiers allowed
final    enum Foo2 { case C } // error: only access modifiers allowed
sealed   enum Foo3 { case C } // error: only access modifiers allowed
implicit enum Foo4 { case C } // error: only access modifiers allowed
lazy     enum Foo5 { case C } // error: only access modifiers allowed
override enum Foo7 { case C } // error: only access modifiers allowed
inline   enum Foo8 { case C } // error: only access modifiers allowed
opaque   enum Foo9 { case C } // error: only access modifiers allowed

enum Foo10 {
  abstract  case C1()  // error: only access modifiers allowed
  final     case C2()  // error: only access modifiers allowed
  sealed    case C3()  // error: only access modifiers allowed
  implicit  case C4()  // error: only access modifiers allowed
  lazy      case C5()  // error: only access modifiers allowed
  override  case C7()  // error: only access modifiers allowed
  private   case C8()  // ok
  protected case C9()  // ok
}

enum Foo11 {
  abstract  case C1  // error: only access modifiers allowed
  final     case C2  // error: only access modifiers allowed
  sealed    case C3  // error: only access modifiers allowed
  implicit  case C4  // error: only access modifiers allowed
  lazy      case C5  // error: only access modifiers allowed
  override  case C7  // error: only access modifiers allowed
  private   case C8  // ok
  protected case C9  // ok
}

enum Foo12 {  // error: Enumerations must contain at least one case
  inline    case C10() // error: only access modifiers allowed
}

final enum Foo13 {   // error: only access modifiers and `into` allowed
  case C1
}

infix enum Foo14[A, B]{   // OK
  case C1 extends Foo14[Int, Int]
  infix case C2 extends Foo14[Int, Int] // error // error
}

into enum Foo15 {   // OK
  case C0
  into case C1  // error // error
}
