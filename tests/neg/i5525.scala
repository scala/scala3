abstract enum Foo1 {} // error: only access modifiers allowed
final    enum Foo2 {} // error: only access modifiers allowed
sealed   enum Foo3 {} // error: only access modifiers allowed
implicit enum Foo4 {} // error: only access modifiers allowed
lazy     enum Foo5 {} // error: only access modifiers allowed
override enum Foo7 {} // error: only access modifiers allowed
inline   enum Foo8 {} // error: only access modifiers allowed
opaque   enum Foo9 {} // error: only access modifiers allowed

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

enum Foo12 {
  inline    case C10() // error: only access modifiers allowed
}