package fpinscala
package applicative

import monads.Functor
import state._
import State._
//import StateUtil._ // defined at bottom of this file
import monoids._
import scala.language.higherKinds
import scala.language.implicitConversions


trait Applicative[F[_]] extends Functor[F] {

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    this.map2(fab, fa){(abFunc: A=>B, a: A) => {
      abFunc(a)
    }
    }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C)=>D): F[D] = {
    val g: A => (B => (C => D)) = f.curried
    val fabcd: F[A=>(B=>(C=>D))] = this.unit(g)
    val fbcd: F[B=>(C=>D)] = this.apply(fabcd)(fa)
    //this.map2(fb, fc)(
    val fcd: F[C=>D] = this.apply(fbcd)(fb)
    val fd: F[D] = this.apply(fcd)(fc)
    fd
  }

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A,B,C,D)=>E): F[E] = {
    val g: A => (B => (C => (D => E))) = f.curried
    val fabcde: F[A => (B => (C => (D => E)))] = this.unit(g)
    val fbcde: F[B => (C => (D => E))] = this.apply(fabcde)(fa)
    val fcde: F[C => (D => E)] = this.apply(fbcde)(fb)
    val fde: F[D=>E] = this.apply(fcde)(fc)
    val fe: F[E] = this.apply(fde)(fd)
    fe
  }

  def sequence[A](fas: List[F[A]]): F[List[A]] = {
    def merge(fa: F[A], flist: F[List[A]]): F[List[A]] =
      this.map2(fa, flist){(a: A, lista: List[A])=>a::lista}
    fas match {
      case (fa: F[A])::(t: List[F[A]]) => merge(fa, sequence(t))
      case _ => this.unit(List[A]())
    }
  }

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = {
    val afb: List[F[B]] = as.map(a => f(a))
    sequence(afb)
  }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    val lfa: List[F[A]] = List.fill(n)(fa)
    sequence(lfa)
  }

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = {
    this.map2(fa, fb){(a: A, b: B) => (a, b)}
  }

  def product[G[_]](G: Applicative[G]):
      Applicative[({type f[x] = (F[x], G[x])})#f] = ???
    // new Applicative[({type f[x] = (F[x], G[x])})#f]{
    //   // implement primites unit and map2
    //   override def map2[A](faa: F[(A,A)], gaa: G[(A,A)])
    //       (merge: ((A,A),(A,A))=>(A,A)): F[(A,B)] = {
    //     val fa3: F[A] = fab1.
    //   }

    // }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = ???

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = ???
}

trait Applicative2[F[_]] extends Applicative[F] {
  override def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]
  override def unit[A](a: => A): F[A]

  override def map[A,B](fa: F[A])(f: A => B): F[B] = {
    val applier: F[A => B] = this.unit(f)
    this.apply(applier)(fa)
  }
  override def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
    // val applier: F[(A,B)=>C] = unit(f)
    // this.apply(applier)(
    val g: A => (B => C) = f.curried
    val applier: F[A=>(B=>C)] = this.unit(g)
    val fbc: F[B=>C] = this.apply(applier)(fa)
    val fc: F[C] = this.apply(fbc)(fb)
    fc
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative2[F] {
  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B]

  //flatMap(mf)(f => map(ma)(a => f(a)))
  override def unit[A](a: => A): Monad[A]

  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = ???

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = ???

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = ???

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
}

object Traverse {
  val listTraverse = ???

  val optionTraverse = ???

  val treeTraverse = ???
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
