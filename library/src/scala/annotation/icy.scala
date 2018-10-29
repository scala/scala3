package scala.annotation

/** An annotation to indicate that a object may be
 *  under initialization, i.e. some fields may not
 *  be assigned yet, and parameters of traits may
 *  not be assigned yet.
 *
 *  When used on methods, it means `this` and `super`
 *  are icy.
 *
 *  When used on constructors, it means the immediate
 *  outer is icy.
 */
class icy extends StaticAnnotation
