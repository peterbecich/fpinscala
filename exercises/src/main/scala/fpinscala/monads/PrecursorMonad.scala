package fpinscala
package monads

import parsing._
import testing._
//import parallelism._
import state._
import parallelism.Nonblocking.Par
//import laziness.Stream


// http://www.scala-lang.org/api/current/#scala.language$
import scala.language.higherKinds
import scala.language.implicitConversions


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
  // Collection's Option
  trait monOption[A] extends Mon[Option] {
    def unit(a: => A): Option[A] = Some(a)
    def flatMap[B](oa: Option[A])(f: A => Option[B]) =
      oa.flatMap(f)
  }

  // object cannot have parametric type??
  // object monOptionAsObject[A] extends Mon[Option] {
  //   def unit(a: => A): Option[A] = Some(a)
  //   def flatMap[B](oa: Option[A])(f: A => Option[B]) =
  //     oa.flatMap(f)
  // }


  /*
   Slightly different syntax
   class
   fpinscala.monads.Mon$$<refinement>
   */
  /*

   /Users/peterbecich/scala/fpinscala/exercises/src/main/scala/fpinscala/monads/Monad.scala:301: object creation impossible, since:
   [error] it has 2 unimplemented members.
   [error] /** As seen from <$anon: fpinscala.monads.Mon[Option]>, the missing signatures are as follows.
   [error]  *  For convenience, these are usable as stub implementations.
   [error]  */
   [error]   def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = ???
   [error]   def unit[A](a: => A): Option[A] = ???
   [error]   def monOptionAsDef[A] = new Mon[Option]{
   [error]                               ^
   [error] 

   parametric types of methods unit and flatMap
   need to match up to trait Mon exactly... flatMap[A,B]
   */
  def monOptionAsDef = new Mon[Option]{
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A,B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
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


