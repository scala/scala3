
import scala.Tuple.Zip

type A
type B
type C

def Test =

  summon[Zip[A *: B *: C *: EmptyTuple, C *: B *: A *: EmptyTuple] =:= (A, C) *: (B, B) *: (C, A) *: EmptyTuple]

  summon[Zip[A *: B *: EmptyTuple, C *: B *: A *: EmptyTuple] =:= (A, C) *: (B, B) *: EmptyTuple]
  summon[Zip[A *: B *: C *: EmptyTuple, C *: B *: EmptyTuple] =:= (A, C) *: (B, B) *: EmptyTuple]

  summon[Zip[A *: B *: C *: Tuple, C *: B *: A *: Tuple] =:= (A, C) *: (B, B) *: (C, A) *: Zip[Tuple, Tuple]]
  summon[Zip[A *: B *: C *: Tuple, C *: B *: A *: Tuple] <:< (A, C) *: (B, B) *: (C, A) *: Tuple]

  summon[Zip[A *: B *: Tuple, C *: B *: A *: Tuple] =:= (A, C) *: (B, B) *: Zip[Tuple, A *: Tuple]]
  summon[Zip[A *: B *: NonEmptyTuple, C *: B *: A *: Tuple] =:= (A, C) *: (B, B) *: Zip[NonEmptyTuple, A *: Tuple]]
  summon[Zip[A *: B *: EmptyTuple, C *: B *: A *: Tuple] =:= (A, C) *: (B, B) *: EmptyTuple]
