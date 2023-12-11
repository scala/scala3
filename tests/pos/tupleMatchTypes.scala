import scala.Tuple.*

type T1; type T2; type T3; type T4; type T5; type T6; type T7; type T8; type T9; type T10; type T11; type T12; type T13; type T14; type T15; type T16; type T17; type T18; type T19; type T20; type T21; type T22; type T23;

type Tup0 = EmptyTuple
type Tup1 = T1 *: EmptyTuple
type Tup2 = (T1, T2)
type Tup22 = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)
type Tup23 = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23)

def testHead: Unit =
  summon[Head[Tup0] =:= Nothing]
  summon[Head[Tup1] =:= T1]
  summon[Head[Tup2] =:= T1]
  summon[Head[Tup22] =:= T1]
  summon[Head[Tup23] =:= T1]

def testLast: Unit =
  summon[Last[Tup0] =:= Nothing]
  summon[Last[Tup1] =:= T1]
  summon[Last[Tup2] =:= T2]
  summon[Last[Tup22] =:= T22]
  summon[Last[Tup23] =:= T23]

def testTail: Unit =
  summon[Tail[Tup0] =:= Nothing]
  summon[Tail[Tup1] =:= EmptyTuple]
  summon[Tail[Tup2] =:= T2 *: EmptyTuple]
  summon[Tail[Tup22] =:= (T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)]
  summon[Tail[Tup23] =:= (T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23)]

def testInit: Unit =
  summon[Init[Tup0] =:= Nothing]
  summon[Init[Tup1] =:= EmptyTuple]
  summon[Init[Tup22] =:= (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)]
  summon[Init[Tup23] =:= (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)]
