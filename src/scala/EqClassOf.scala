package scala

/** A class providing a specialized notion of equality, allowing
 *  only values in the same equality class to be compared.
 */
trait EqClassOf[-T] extends Any with EqClass

