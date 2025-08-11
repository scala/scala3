package dummy

import language.experimental.captureChecking
import caps.*

trait Nested:
  val c: AnyRef^
  val next: Nested

trait Arrows:
  val a: AnyRef^
  val b: AnyRef^
  val c: AnyRef^

  val purev: Int -> Int
  val purev2: Int ->{} Int
  val impurev: Int => Int
  val impurev2: Int ->{a,b,c} Int
  val impurev3: Int ->{a,b,c} Int => Int
  val impureCap: Int ->{cap} Int
  val impureCap2: Int ->{cap, a, b, c} Int
  val contextPureV: Int ?-> Int
  val contextPureV2: Int ?->{} Int
  val contextImpureV: Int ?=> Int
  val contextImpureV2: Int ?->{a,b,c} Int
  val contextImpureV3: Int ?->{a,b,c} Int ?=> Int
  val contextImpureCap: Int ?->{cap} Int
  val contextImpureCap2: Int ?->{cap, a, b, c} Int

  def pure(f: Int -> Int): Int
  def pure2(f: Int ->{} Int): Int
  def impure(f: Int => Int): Int
  def impure2(f: Int ->{a,b,c} Int): Int
  def impure3(f: Int ->{a,b,c} Int => Int): Int

  def consumes(@consume a: AnyRef^): Any
  def consumes2(@consume x: AnyRef^{a}, @consume y: AnyRef^{b}): Any

  def reachThis: AnyRef^{this*}

  def byNamePure(f: -> Int): Int
  def byNameImpure(f: ->{a,b,c} Int): Int
  def byNameImpure2(f: => Int): Int

  def pathDependent(n: Nested^)(g: AnyRef^{n.c} => Any): Any
  def pathDependent2(n: Nested^)(g: AnyRef^{n.next.c} => Any): Any
  def pathDependent3(n: Nested^)(g: AnyRef^{n.c} => AnyRef^{n.next.c} ->{n.c} Any): Any
  def pathDependent4(n: Nested^)(g: AnyRef^{n.c} => AnyRef^{n.next.c} ->{n.c} Any): AnyRef^{n.next.next.c}
  def pathDependent5(n: Nested^)(g: AnyRef^{n.c} => AnyRef^{n.next.c} ->{n.c} Any): AnyRef^{n.next.next.c*, n.c, cap}

  def contextPure(f: AnyRef^{a} ?-> Int): Int
  def contextImpure(f: AnyRef^{a} ?=> Int): Int
  def contextImpure2(f: AnyRef^{a} ?->{b,c} Int): Int
  def contextImpure3(f: AnyRef^{a} ?->{b,c} Int => AnyRef^{a} ?=> Int): Int

  val noParams: () -> () -> Int
  val noParams2: () ->{} () ->{} Int
  val noParamsImpure: () => () => Int => Unit

  val uncurried: (x: AnyRef^, y: AnyRef^) -> AnyRef^{x,y} => Int ->{x,y} Int
  val uncurried2: (x: AnyRef^, y: AnyRef^) -> AnyRef => Int ->{x,y} Int
  val uncurried3: (x: AnyRef^, y: AnyRef^) => AnyRef
  val uncurried4: (x: AnyRef^, y: AnyRef^) ->{a,b} AnyRef^ => Int ->{x,y} Int

  val contextUncurried: (x: AnyRef^{a}, y: AnyRef^{b}) ?-> AnyRef^{x,y} ?-> Int ?->{x,y} Int
  val contextUncurried2: (x: AnyRef^{a}, y: AnyRef^{b}) ?-> AnyRef ?-> Int ?->{x,y} Int
  val contextUncurried3: (x: AnyRef^{a}, y: AnyRef^{b}) ?=> AnyRef
  val contextUncurried4: (x: AnyRef^{a}, y: AnyRef^{b}) ?->{a,b} AnyRef^ ?=> Int ?->{x,y} Int

  def polyPure[A](f: A -> Int): Int
  def polyPure2[A](f: A ->{} Int): Int
  def polyImpure[A](f: A => Int): Int
  def polyImpure2[A](f: A ->{a,b,c} Int): Int
  def polyImpure3[A](f: A ->{a,b,c} Int => Int): Int

  def polyContextPure[A](f: A ?-> Int): Int
  def polyContextPure2[A](f: A ?->{} Int): Int
  def polyContextImpure[A](f: A ?=> Int): Int
  def polyContextImpure2[A](f: A ?->{a,b,c} Int): Int
  def polyContextImpure3[A](f: A ?->{a,b,c} Int => Int): Int

  val polyPureV: [A] => A -> Int
  val polyPureV2: [A] => Int => A ->{a,b,c} Int
  val polyImpureV: [A] -> A => Int
  val polyImpureV2: [A] -> A => Int