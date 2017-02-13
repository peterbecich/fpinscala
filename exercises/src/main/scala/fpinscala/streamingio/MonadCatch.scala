package fpinscala.streamingio

import fpinscala.iomonad._
import scala.language.higherKinds

/*
 * A context in which exceptions can be caught and
 * thrown.

 Don't forget to implement Monad primitives unit and flatMap
 (for this particular Monad trait)
 */
trait MonadCatch[F[_]] extends Monad[F] {
  def attempt[A](a: F[A]): F[Either[Throwable,A]]
  def fail[A](t: Throwable): F[A]

  def map2[A,B,C](fa: F[A], fb: F[B])(abc: (A,B) => C): C =
    this.bind(fa)((a: A) => this.map(fb)((b: B) => abc(a,b)))

}

object MonadCatch {
  implicit def taskMonadCatch = new MonadCatch[Task] {
    def unit[A](a: => A): Task[A] = Task.unit(a)
    def flatMap[A,B](a: Task[A])(f: A => Task[B]): Task[B] = a.flatMap(f)
    def attempt[A](a: Task[A]): Task[Either[Throwable,A]] = a.attempt
    def fail[A](err: Throwable): Task[A] = Task.fail(err)
  }
}
