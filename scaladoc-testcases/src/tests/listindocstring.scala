package tests
package listindocstring

/**
 * These are useful methods that exist for both $some and $none.
 *  - [[isDefined]] — True if not empty
 *  - [[isEmpty]] — True if empty
 *  - [[nonEmpty]] — True if not empty
 *  - [[orElse]] — Evaluate and return alternate optional value if empty
 *  - [[getOrElse]] — Evaluate and return alternate value if empty
 *  - [[get]] — Return value, throw exception if empty
 *  - [[fold]] —  Apply function on optional value, return default if empty
 *  - [[map]] — Apply a function on the optional value
 *  - [[flatMap]] — Same as map but function must return an optional value
 *  - [[foreach]] — Apply a procedure on option value
 *  - [[collect]] — Apply partial pattern match on optional value
 *  - [[filter]] — An optional value satisfies predicate
 *  - [[filterNot]] — An optional value doesn't satisfy predicate
 *  - [[exists]] — Apply predicate on optional value, or false if empty
 *  - [[forall]] — Apply predicate on optional value, or true if empty
 *  - [[contains]] — Checks if value equals optional value, or false if empty
 *  - [[zip]] — Combine two optional values to make a paired optional value
 *  - [[unzip]] — Split an optional pair to two optional values
 *  - [[unzip3]] — Split an optional triple to three optional values
 *  - [[toList]] — Unary list of optional value, otherwise the empty list
 */
trait O

/**
 * Some text
 *
 *
 * Next paragraph
 *
 *
 * Last paragraph
 */
trait K
