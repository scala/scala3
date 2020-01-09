
erased   enum Foo6 {} // error: only access modifiers allowed

enum Foo10 {
  erased    case C6()  // error: only access modifiers allowed
}

enum Foo11 {
  erased    case C6  // error: only access modifiers allowed
}
