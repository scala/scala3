class T1[F[_]]
class T2[F[_]]
class T3[F[_]]
class T4[F[_]]
class T5[F[_]]
class T6[F[_]]
class T7[F[_]]
class T8[F[_]]
class T9[F[_]]
class T10[F[_]]
class T11[F[_]]
class T12[F[_]]
class T13[F[_]]
class T14[F[_]]
class T15[F[_]]
class T16[F[_]]
class T17[F[_]]
class T18[F[_]]
class T19[F[_]]
class T20[F[_]]
class T21[F[_]]
class T22[F[_]]

class Result[F[_]: {T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22}]

val r = for
  t1 <- Option(new T1[Option])
  t2 <- Option(new T2[Option])
  t3 <- Option(new T3[Option])
  t4 <- Option(new T4[Option])
  t5 <- Option(new T5[Option])
  t6 <- Option(new T6[Option])
  t7 <- Option(new T7[Option])
  t8 <- Option(new T8[Option])
  t9 <- Option(new T9[Option])
  t10 <- Option(new T10[Option])
  t11 <- Option(new T11[Option])
  t12 <- Option(new T12[Option])
  t13 <- Option(new T13[Option])
  t14 <- Option(new T14[Option])
  t15 <- Option(new T15[Option])
  t16 <- Option(new T16[Option])
  t17 <- Option(new T17[Option])
  t18 <- Option(new T18[Option])
  t19 <- Option(new T19[Option])
  t20 <- Option(new T20[Option])
  t21 <- Option(new T21[Option])
  t22 <- Option(new T22[Option])
  given T1[Option] = t1
  given T2[Option] = t2
  given T3[Option] = t3
  given T4[Option] = t4
  given T5[Option] = t5
  given T6[Option] = t6
  given T7[Option] = t7
  given T8[Option] = t8
  given T9[Option] = t9
  given T10[Option] = t10
  given T11[Option] = t11
  given T12[Option] = t12
  given T13[Option] = t13
  given T14[Option] = t14
  given T15[Option] = t15
  given T16[Option] = t16
  given T17[Option] = t17
  given T18[Option] = t18
  given T19[Option] = t19
  given T20[Option] = t20
  given T21[Option] = t21
  given T22[Option] = t22
  result <- Option(new Result[Option])
yield result