package scala.annotation

// TODO: Write the actual scaladoc of this annotation

// The annotation from Java is a simple hack as both have the same semantic
// meaning, only this one should be used by Scala code and the second
// annotation by Java Code
// @documented (TODO: can we make it work without breaking the cycle detection algorithm?)
@java.lang.annotation.Documented
final class documented extends StaticAnnotation
