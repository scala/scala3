abstract enum Foo1 {} // error: only access modifiers allowed
final    enum Foo2 {} // error: only access modifiers allowed
sealed   enum Foo3 {} // error: only access modifiers allowed
implicit enum Foo4 {} // error: only access modifiers allowed
lazy     enum Foo5 {} // error: only access modifiers allowed
erased   enum Foo6 {} // error: only access modifiers allowed
override enum Foo7 {} // error: only access modifiers allowed
inline   enum Foo8 {} // error: only access modifiers allowed
opaque   enum Foo9 {} // error: only access modifiers allowed

enum Foo10 {
  abstract  case C1()  // error: no modifier allowed
  final     case C2()  // error: no modifier allowed
  sealed    case C3()  // error: no modifier allowed
  implicit  case C4()  // error: no modifier allowed
  lazy      case C5()  // error: no modifier allowed
  erased    case C6()  // error: no modifier allowed
  override  case C7()  // error: no modifier allowed
  private   case C8()  // error: no modifier allowed
  protected case C9()  // error: no modifier allowed
  inline    case C10() // error: no modifier allowed
  opaque    case C11() // error: no modifier allowed
}

enum Foo11 {
  abstract  case C1  // error: no modifier allowed
  final     case C2  // error: no modifier allowed
  sealed    case C3  // error: no modifier allowed
  implicit  case C4  // error: no modifier allowed
  lazy      case C5  // error: no modifier allowed
  erased    case C6  // error: no modifier allowed
  override  case C7  // error: no modifier allowed
  private   case C8  // error: no modifier allowed
  protected case C9  // error: no modifier allowed
  inline    case C10 // error: no modifier allowed
  opaque    case C11 // error: no modifier allowed
}
