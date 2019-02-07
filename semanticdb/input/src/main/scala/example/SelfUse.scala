package example

class SelfUse extends B { a : B =>
val c = a.b
}