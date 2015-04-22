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


/* Seeing F[_] confused me as I had assumed any variable outside of brackets could not be
parametric.
That is not the case.

F is the parametric variable, and the variable inside the brackets is a "don't care"

F's place can be taken by any parametric type
List[A] can take F's place
Int cannot take F's place
 */
trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  // i.e. fab = List[(Int, String)]
  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) = {
    //(map(fab)(_._1), map(fab)(_._2))
    // more explicitly

    // note this won't compile
    // ***object*** Functor does not take type parameters
    // instance of trait Functor has type parameters already set
    // blank trait Functor will take type parameters???
    //val first = Functor[F[(A,B)]].map(
    val first: F[A] = this.map(fab){
      (abTuple: (A,B)) => abTuple._1
    }
    val second: F[B] = this.map(fab){
      (abTuple: (A,B)) => abTuple._2
    }
    // where 'this' is an instance of trait Functor 
    // with map assumed defined
    (first, second)
  }
  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
  // List[A] takes no type parameters??
  // def listFunctionAlt[A] = new Functor[List[A]] {
  def listFunctionAlt[A] = new Functor[List] {
    def map[B](as: List[A])(f: A => B): List[B] = as map f
  }
}


/*
 Instances of Mon will look like Mon[List[A]]
 */
trait Mon[F[_]] {
  def unit[A](a: => A): F[A]   // why are these methods not returning a Functor?


  def map[A,B](fa: F[A])(f: A => B): F[B] = {
    // same mistake made here as in map2;
    // we are using the methods of Mon, not F
    // fa.flatMap(
    //   (a: A) => F[B].unit(f(a))
    // )


    // this: Mon[F]  where F's internal type is anything!
    // F[Int], F[String], etc
    this.flatMap(fa){
      (a: A) => this.unit(f(a))
    }

  }
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]


  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
    // this isn't working because fa and fb are unknown to have flatMap
    // ***however***, Mon[F[A]] and Mon[F[B]] have flatMap
    // fa.flatMap((a: A) =>
    //   fb.flatMap((b: B) =>  // use Map, f does not provide functor container
    //     F[C].unit(f(a,b))
    //   )
    // )



    // trait
    // fpinscala.monads.Mon
    // (companion)

    // trait
    // fpinscala.monads.Mon[F]
    // ---------------------------
    // flatMap		    (F[A]) => (Function1[A, F[B]]) => F[B]
    // map		    (F[A]) => (Function1[A, B]) => F[B]
    // map2		    (F[A], F[B]) => (Function2[A, B, C]) => F[C]
    // unit		    (<byname>[A]) => F[A]



    // this.flatMap(fa: F[A])(f: Function1[A, F[C]]):
    // (fpinscala.monads.F[A]) =>   // notice the currying
    // (scala.Function1[A, F[C]]) => 
    // fpinscala.monads.F[C]

    /* 
     Where is our Mon[F[B]] to work with?
     There is no Mon[F[B]], nor Mon[F[A]]!
     There is only Mon[F[_]]
     */
    this.flatMap(fa){
      (a: A) => this.map(fb){
        (b: B) => f(a,b)
      }
    }: F[C]

  }
}

object Mon {
  // Option[A] takes no type parameters?
  // "Option takes no type parameters" would make more sense to me
  // def monOption[A]: Mon[Option[A]] =

  // It is because the signature of Mon[F[_]] is a "don't care"
  // for the internal type of F

  /*
   trait
   fpinscala.monads.Mon$$monOption[A]
   */
  trait monOption[A] extends Mon[Option] {
    def unit(a: => A): Option[A] = Some(a)
    def flatMap[B](oa: Option[A])(f: A => Option[B]) =
      oa.flatMap(f)
  }


  /*
   Slightly different syntax
   class
   fpinscala.monads.Mon$$<refinement>
   */
  def monOptionAsDef[A] = new Mon[Option]{
    def unit(a: => A): Option[A] = Some(a)
    def flatMap[B](oa: Option[A])(f: A => Option[B]) =
      oa.flatMap(f)
  }

  // doesn't compile
  // val monOptionAsVal[A] = new Mon[Option]{
  //   def unit(a: => A): Option[A] = Some(a)
  //   def flatMap[B](oa: Option[A])(f: A => Option[B]) =
  //     oa.flatMap(f)
  // }


  trait monList[A] extends Mon[List] {
    def unit(a: => A): List[A] = List.apply(a)
    def flatMap[B](oa: List[A])(f: A => List[B]): List[B] =
      oa.flatMap(f): List[B]
    }
  /* 
   not all types that we define Mon for 
   will already have flatMap and unit methods defined
   */


}


// for example,
// Monad[List[A]] extends Functor[List]
trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] // note Monad is a TRAIT

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    this.flatMap(ma)(a => this.unit(f(a)))

  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    this.flatMap(ma)(a => this.map(mb)(b => f(a, b)))

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

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] =
      ma flatMap f
  }

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

