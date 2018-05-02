package scala.annotation

/** An annotation to indicate that a value may be
 *  under initialization, but all its fields are
 *  assigned.
 *
 *  All fields of a filled object are assigned, but
 *  the fields may refer to objects under initialization.
 *  In contrast, a partial object may have unassigned
 *  fields.
 *
 *  When used on methods, it means `this` and `super`
 *  are filled.
 *
 *  When used on constructors, it means the immediate
 *  outer is filled.
 */
class filled extends StaticAnnotation
