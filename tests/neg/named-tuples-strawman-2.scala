import compiletime.*
import compiletime.ops.int.*
import compiletime.ops.boolean.!

object NamedTupleDecomposition:
  import NamedTupleOps.*

  /** The names of the named tuple type `NT` */
  type Names[NT <: AnyNamedTuple] <: Tuple = NT match
    case NamedTuple[n, _] => n

  /** The value types of the named tuple type `NT` */
  type DropNames[NT <: AnyNamedTuple] <: Tuple = NT match
    case NamedTuple[_, x] => x

object NamedTupleOps:

  opaque type AnyNamedTuple = Any

  opaque type NamedTuple[N <: Tuple, +X <: Tuple] >: X <: AnyNamedTuple = X

  export NamedTupleDecomposition.*

  object NamedTuple:
    def apply[N <: Tuple, X <: Tuple](x: X): NamedTuple[N, X] = x

  extension [NT <: AnyNamedTuple](x: NT)
    inline def toTuple: DropNames[NT] = x.asInstanceOf // error
    inline def names: Names[NT] = constValueTuple[Names[NT]]
