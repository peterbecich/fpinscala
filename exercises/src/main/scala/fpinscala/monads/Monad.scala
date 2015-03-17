package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._


/*
 The three monad laws

 associativity:
 m flatMap f flatMap g == m flatMap (x => f(x) flatMap g)

 left unit:
 unit(x) flatMap f == f(x)

 right unit:
 m flatMap unit == m

 Show that Option is a monad

 abstract class Option[+T] {
   def flatMap[U](f: T => Option[U]): Option[U] = this match {
     case Some(x) => f(x)
     case None => None
   }
 }


 
 Show left unit law:
 In Option, Unit is Some

 Where is it shown that f has signature (T => Option[U])?
  Some(x) flatMap f
 =f(x)

 Show right unit law:
  opt flatMap Some
 =opt match {
    case Some(x) => Some(x)
    case None => None
  }
 =opt

 Show associative law:
 
  opt flatMap f flatMap g

  Prove for both possibilities of parethesis placement ... or not
  (opt flatMap f) flatMap g
 =(opt match {
    case Some(x) => f(x)
    case None => None
 } ) match {
    case Some(y) => g(y)
    case None => None
 }
 =(opt match {
    case Some(x) => f(x) match {  // Analogous to algebraic distribution?
      case Some(y) => g(y)
      case None => None
    }
    case None => None match {
      case Some(y) => g(y)
      case None => None
    }
 } 
) 
=opt match {
    case Some(x) => f(x) match {
      case Some(y) => g(y)
      case None => None
    }
    case None => None
 } 

                 aside:  f(x) match {
                           case Some(y) => g(y)
                           case None => None
                         } = (x => f(x) flatMap g)
=opt match {
    case Some(x) => f(x) flatMap g
    case None => None
 } 
=opt flatMap (x => f(x) flatMap g)




Consequentially...

Associativity says that one can "inline" nested for-expressions

 for (y <- for (x <- m; y <- f(x)) yield y
     z <- g(y)) yield z
=for (x <-m;
      y <-f(x);
      z <-g(y)) yield z

right unit 
 for (x <- m) yield x
=m

left unit does not have an analogue for for-expressions



example

abstract class Try[+T]
case class Success[T](x: T) extends Try[T]
case class Failure(ex: Exception) extends Try[Nothing]

object Try {
  def apply[T](expr: => T): Try[T] = 
    try Success(expr)  // uses Java try
    catch {
      case NonFatal(ex) => Failure(ex)
      // fatal exception not necessary to catch
    }

  def flatMap[U](f: T => Try[U]): Try[U] = this match {
    case Success(x) => try f(x) catch { case NonFatal(ex) => Failure(ex) }
    case fail: Failure => fail
  }
  def map[U](f: T => U): Try[U] = this match {
    case Success(x) => Try(f(x))
    case fail: Failure => fail
  }
 }

is Try a Monad?

left unit
show apply(x) flatMap f = f(x)


   
 */

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] // note Monad is a TRAIT

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = ??? // lma match {
    // case h::t => 


  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = ???

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = ???

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = ???
//    (a: A) => f(a).flatMap(g)


  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???


  def join[A](mma: M[M[A]]): M[A] = ??? //mma.flatMap((ma: M[A]) => 

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = ???

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = ???

  val optionMonad: Monad[Option] = ???

  val streamMonad: Monad[Stream] = ???

  val listMonad: Monad[List] = ???

  def stateMonad[S] = ???

  val idMonad: Monad[Id] = ???

  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = ???
  def flatMap[B](f: A => Id[B]): Id[B] = ???
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = ???
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = ???
  }
}

