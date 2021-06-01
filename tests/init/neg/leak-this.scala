class Parent {
    val child: Child = new Child(this)
}

class Child(parent: Parent) {
    val friend = new Friend(this.parent)
}

class Friend(parent: Parent) {
    val tag = 10
}
