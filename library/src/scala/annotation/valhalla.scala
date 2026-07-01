package scala.annotation

import scala.language.`2.13`

/** A class annotation which verifies that the class will be a
 * Project Valhalla value class.
 *
 * If it is present, the compiler will issue an error if the class
 * does not satisfy the requirements of the Project Valhalla value
 * class as specified in https://openjdk.org/jeps/401.
 */

@scala.annotation.experimental
final class valhalla extends StaticAnnotation
