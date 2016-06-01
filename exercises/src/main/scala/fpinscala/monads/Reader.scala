package fpinscala.monads

import scala.language.higherKinds

// https://www.youtube.com/watch?v=H3CCvXx4GvI
case class ReaderT[F[_], A, B](run: A => F[B]) {

  def map[C](f: B => C)(implicit F: Functor[F]): ReaderT[F, A, C] = ReaderT {
    (a: A) => F.map(run(a))(f)
  }

  def flatMap[C](f: B => ReaderT[F, A, C])(implicit F: Monad[F]): ReaderT[F, A, C] = ReaderT {
    (a: A) =>
    val fb: F[B] = run(a)
    val freader: F[ReaderT[F, A, C]] = F.map(fb){(b: B) => f(b)}
    ???
  }

  def andThen[C](that: ReaderT[F, B, C])(implicit F: Monad[F]): ReaderT[F, A, C] = ???

  def compose[C](that: ReaderT[F, C, A])(implicit F: Monad[F]): ReaderT[F, C, B] = ???

}

object ReaderT {

  def ask[F[_], A](implicit F: Monad[F]): ReaderT[F, A, A] =
    ReaderT { a => F.unit(a) }

}
