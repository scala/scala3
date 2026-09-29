// nopos-error
// nopos-error
val child: broken.Child = ???

// Where
// Child.java
// ```java
// package broken;
// public class Child extends Outer.Inner {}
// ```
// and
// Outer.java
// ```java
// package broken;
// public class Outer {
//   public static class Inner {}
// }
// ```
//
// Testing ClassfileParser doesn't crash on
// missing outer class's classfile
