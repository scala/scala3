package theproblem

import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.ExecutionContext.Implicits.global

object Items:
  opaque type TheId = Int
  extension (TheId: TheId) def raw: Int = TheId

  object TheId:
    def apply(id: Int): TheId = id


import Items.TheId

case class AnError(id: TheId)

type ErrAcc[A] = Either[Seq[AnError], A]

case class Res[A](future: Future[ErrAcc[A]]):

  def map[B](f: A => B)(using ExecutionContext): Res[B] =
    Res(this.future.map(_.map(f)))

  def flatMap[B](f: A => Res[B])(using ExecutionContext): Res[B] =
    Res(this.future.flatMap {
      case Right(x) => f(x).future
      case Left(es) => Future.successful(Left[Seq[AnError], B](es))
    })

  def zip[B](that: Res[B])(using ExecutionContext): Res[(A, B)] =
    def zipacc(a: ErrAcc[A], b: ErrAcc[B]): ErrAcc[(A, B)] = (a, b) match
      case (Right(x), Right(y)) => Right((x, y))
      case (Right(_), Left(e) ) => Left(e)
      case (Left(e),  Right(_)) => Left(e)
      case (Left(ex), Left(ey)) => Left(ex ++ ey)

    Res(this.future.flatMap { a => that.future.map { b => zipacc(a, b) } })

object Res:
  def successful[A](x: A): Res[A] = Res(Future.successful(Right[Seq[AnError], A](x)))

  def traverse[A, B](as: Seq[A])(f: A => Res[B])(using ExecutionContext): Res[Seq[B]] =
    as.foldLeft[Res[Seq[B]]](successful(Seq.empty)) { (acc, x) => acc.zip(f(x)).map(_ :+ _) }

trait M:
  def getID(name: String)(using ExecutionContext): Res[TheId]
  def bfs(sid: TheId, tid: TheId): Res[Boolean]

class theproblem(m: M):

  def thebug(names: List[String]): Res[Seq[(String, String, Boolean)]] =
    Res.traverse(names)(m.getID).flatMap { ids =>
      val id_names = ids.zip(names)
      val ps = for {
        (sid, sname) <- id_names
        (tid, tname) <- id_names
        if sid != tid
      } yield (sid -> sname, tid -> tname)
      Res.traverse(ps){ (s, t) =>
        m.bfs(s(0), t(0)).map((s(1), t(2), _))  // error t(2) should be t(1)
      }
    }