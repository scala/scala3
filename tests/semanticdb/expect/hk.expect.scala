package hk

trait Monad/*<-hk::Monad#*/[M/*<-hk::Monad#[M]*/[_]] {
  def pure/*<-hk::Monad#pure().*/[A/*<-hk::Monad#pure().[A]*/](a/*<-hk::Monad#pure().(a)*/: A/*->hk::Monad#pure().[A]*/): M/*->hk::Monad#[M]*/[A/*->hk::Monad#pure().[A]*/] = ???/*->scala::Predef.`???`().*/
  def flatMap/*<-hk::Monad#flatMap().*/[A/*<-hk::Monad#flatMap().[A]*/, B/*<-hk::Monad#flatMap().[B]*/](m/*<-hk::Monad#flatMap().(m)*/: M/*->hk::Monad#[M]*/[A/*->hk::Monad#flatMap().[A]*/])(f/*<-hk::Monad#flatMap().(f)*/: A/*->hk::Monad#flatMap().[A]*/ => M/*->hk::Monad#[M]*/[B/*->hk::Monad#flatMap().[B]*/]): M/*->hk::Monad#[M]*/[B/*->hk::Monad#flatMap().[B]*/] = ???/*->scala::Predef.`???`().*/
}

class EitherMonad/*<-hk::EitherMonad#*/[T/*<-hk::EitherMonad#[T]*/] extends Monad/*->hk::Monad#*/[[E/*<-hk::EitherMonad#`<init>`().[E]*/] =>> Either/*->scala::package.Either#*/[T/*->hk::EitherMonad#[T]*/, E]] {
}

type MapKV/*<-hk::hk$package.MapKV#*/ = [K/*<-hk::hk$package.MapKV#[K]*/] =>> [V/*<-hk::hk$package.MapKV#[V]*/] =>> Map/*->scala::Predef.Map#*/[K/*->hk::hk$package.MapKV#[K]*/,V/*->hk::hk$package.MapKV#[V]*/]

type MapV/*<-hk::hk$package.MapV#*/ = [_] =>> [V/*<-hk::hk$package.MapV#[V]*/] =>> Map/*->scala::Predef.Map#*/[String/*->scala::Predef.String#*/, V/*->hk::hk$package.MapV#[V]*/]

type MapEither/*<-hk::hk$package.MapEither#*/ = [K/*<-hk::hk$package.MapEither#[K]*/] =>> [L/*<-hk::hk$package.MapEither#[L]*/] =>> [R/*<-hk::hk$package.MapEither#[R]*/] =>> Map/*->scala::Predef.Map#*/[K/*->hk::hk$package.MapEither#[K]*/, Either/*->scala::package.Either#*/[L/*->hk::hk$package.MapEither#[L]*/, R/*->hk::hk$package.MapEither#[R]*/]]

type Id/*<-hk::hk$package.Id#*/[A/*<-hk::hk$package.Id#[A]*/] = A/*->hk::hk$package.Id#[A]*/
