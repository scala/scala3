package scala.annotation

/** An annotation that marks a method as taking non-hot parameters.
 *  The initialization checker will not require all parameters to be hot at call-site,
 *  opting to run a more expensive analysis.
 */
final class nonHotParameters extends StaticAnnotation
