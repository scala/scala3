import language.experimental.captureChecking

class Data

class Parent(
    name: String,
    op: () => Unit
)

class Child(
    op: Data^
) extends Parent("abab", () => (op: Unit))