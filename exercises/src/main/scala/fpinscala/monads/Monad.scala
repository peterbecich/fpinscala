package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._

import scala.language.higherKinds

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

  // Functor lacks flatMap, so cannot implement map2
  // def map2[A,B,C]

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
  trait listFunctorAlt[A] extends Functor[List] {
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


// for example,
// Monad[List[A]] extends Functor[List]
// Functor's parametric signature, F[_] will *not* accept
// M[A], List[A], List[Int], Option[A], etc...
// Only M, List, Option,... is correct.
trait Monad[M[_]] extends Functor[M] {
  /*
   Functor[M] lacks a unit method.
   Unit needs to be abstractly implemented here.

   Defining 'apply' would be appropriate for producing a Monad, right here.
   Defining 'unit' is appropriate for producing an instance of the
   internal type of Monad, M[_].

   In other words, 'unit' produces an instance of a type "one level in",
   not this type Monad.
   */


  /*
   Three known sets of primitive methods, left abstract in the Monad trait.  Three potential distinct Monad traits.
   unit and flatMap
   unit and compose
   unit, map, and join

   */

  // unit and flatMap left abtract, as they were in Mon
  def unit[A](a: => A): M[A]

  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    this.flatMap(ma)(a => this.unit(f(a)))

  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    this.flatMap(ma)(a => this.map(mb)(b => f(a, b)))

  /*
   Sequence confused me because it is only useful with Lists.
   It is an "odd duck" where other methods of Monad are very general.
   Because it is so restricted, feel free to use the methods of List,
   which is certainly a concrete type, unlike F or M.
   */
  def sequence[A](lma: List[M[A]]): M[List[A]] = {
    /* Type erasure issue
     http://stackoverflow.com/questions/1094173/how-do-i-get-around-type-erasure-on-scala-or-why-cant-i-get-the-type-paramete
     type M[A] of h is ***unenforced***
     Since this is only a warning, I can keep these type annotations
     for my own clarity
     case (h: M[A])::(t: List[M[A]]) => this.unit{
     
     Monad.flatMap has independent parametric types A and B.
     It's not appropriate to define parametric A in the signature of
     trait Monad because Monad operates on a Functor of any internal type.
     */

    this.traverse(lma){(ma: M[A]) => {
      ma
    }: M[A]
    }: M[List[A]]
  }

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = {
    /*
     Going to use fold methods of List functor
     la.foldRight(z: B)(op: Function2[A, B, B])
     There are no fold methods implemented (yet) for Monad
     */
    la.foldRight(this.unit(List[B]())){(a: A, mlb: M[List[B]]) => {
      // (A, M[List[B]]) => M[List[B]]
      val composition: A => M[List[B]] = this.compose(f,
        // (B, M[List[B]]) => M[List[B]]
        (b: B) => {
          this.map(mlb){
            (lb: List[B]) => b::lb
          }
        }
      )
      composition(a)
    }: M[List[B]]
    }: M[List[B]]
  }: M[List[B]]


  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    val lma: List[M[A]] = List.fill(n)(ma)
    this.sequence(lma)
  }

  /* Implement without flatMap, because flatMap will be implemented
   with this.
   An alternative set of primitives are compose and unit.
   I thought primitive implied "left abstract"...
   */
  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = {
    (a: A) => {
      this.flatMap(f(a))(g)
    }
  }


  def join[A](mma: M[M[A]]): M[A] = {
    /*
     need function M[A] => A
     Or not...

     It is easier to eliminate the outer Functor.
     I initially tried to eliminate the inner Functor.
     */
    this.flatMap(mma){(ma: M[A]) =>
      ma
    }
  }

  def product[A,B](ma: M[A], mb: M[B]): M[(A, B)] =
    this.map2(ma, mb)((a: A, b: B) => (a,b): Tuple2[A,B])

  /*
   Imagine the uses of filterM.
   ms = List(1,2,3,4)
   f: A => List[Boolean] = a > 2

   ms = List(1,2,3,4)
   f: A => Option[Boolean]   ???????

   */
  // def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {


  // }
}



object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  // val parMonad: Monad[Par] = new Monad[Par] {
  //   def unit[A](a: => A): Par[A] = Par.unit(a)
  //   override def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] =
  //need to fix implementation of Par
  //     ma flatMap f
  // }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = ???

  val optionMonad: Monad[Option] = ???

  val streamMonad: Monad[Stream] = ???

  val listMonad: Monad[List] = new Monad[List] {
    // remember that the signature of 'unit' is the same between
    // all monad instances (where the return type is Functor[A]
    // def unit[A]: List[A] = List.empty
    def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A,B](la: List[A])(fa: A => List[B]) =
      la.flatMap(fa)
  }

  // see section 11.5.2
  // case class State[S,+A](run: S => (A, S))
  // fpinscala.state.State takes two type parameters,
  // expected: one
  // val stateMonad[S] = new Monad[State[_,Int]] {
  //   def unit[S](s: => S): State[S]
  //   def flatMap[
  //   }

  type IntState[A] = State[Int, A]

  def intStateMonad[S] = new Monad[IntState] {
    def unit[A](a: => A): IntState[A] = State.unit(a)
    def flatMap[A, B](st: IntState[A])(f: A => IntState[B]): 
        IntState[B] = {
      st.flatMap(f)
    }
  }

  // why the funny parenthesis pattern,  ({...})   ?
  def intStateMonadAlt[S] = 
    new Monad[({type IntStateAlt[A] = State[Int, A]})#IntStateAlt] {
    def unit[A](a: => A): State[Int, A] = State.unit(a)
    def flatMap[A, B](st: State[Int, A])(f: A => State[Int, B]): 
        State[Int, B] = {
      st.flatMap(f)
    }
  }



  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A,B](ida: Id[A])(f: A => Id[B]): Id[B] = {
      ida.flatMap(f)
    }
  }

  /*
   reader monad defined twice to demonstrate inline creation of
   an "anonymous" type.
   See anonymous type 'f' below
   */
  // def readerMonad[R] = new Monad[Reader] {
  //   //override def unit[A]
  // }

}
object MonadTest {
  //import fpinscala.monads.Monad
  def main(args: Array[String]): Unit = {
    // val ll = (10 to 20).toList
    // val ll2 = (10 to 20).toList
    val ll = List(1,2,3,4,5,6)
    val ll2 = List(4,5,6,7,8,9)
    val mapped = Monad.listMonad.map(ll)((i: Int) => i + 1)
    // println(ll)
    // println(mapped)
    //val llProduct = Monad.listMonad
    //.product(ll, ll2)


  }
}

/*
 A monad trait based on the second primitive set of methods
 to be left abstract: unit and compose
 */
trait MonadB[M[_]] extends Monad[M] {
  def unit[A](a: => A): M[A]
  override def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C]

  override def map[A,B](ma: M[A])(f: A => B): M[B] =
    this.flatMap(ma)(a => this.unit(f(a)))

  override def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    this.flatMap(ma)(a => this.map(mb)(b => f(a, b)))


  /*
   Implement in terms of `compose`
   You should be able to implement this and all other combinators
   with only `unit` and `compose`.
   
   */
  override def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = {
    //this.compose(f: Function1[A, M[B]], g: Function1[B, M[C]]):
    // A => M[C]
    // compose( C => M[A], A => M[B] )(c: C)
    // not necessary to make up type C
    // what would an instance of C even look like?
    // I could also use a known instance, like a char or an int
    //this.compose((_:Int)=>ma, f)(5)
    //this.compose((_:Any)=>ma, f)()
    this.compose((_: Unit)=>ma, f)()
    // book answer prefers Unit to Any
  }




}


/*
 A monad trait based on the third primitive set of methods
 to be left abstract: unit, map, and join
 */
trait MonadC[M[_]] extends Monad[M] {
  def unit[A](a: => A): M[A]

  override def map[A,B](ma: M[A])(f: A => B): M[B]

  override def join[A](mma: M[M[A]]): M[A]

  override def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    this.flatMap(ma)(a => this.map(mb)(b => f(a, b)))

  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = {
    this.join(this.map(ma)(f))
  }

  override def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = {
    (a: A) => {
      val mb = f(a)
      val mc = this.flatMap(mb)(g)
      mc
    }
  }

}


case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = ???
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = ???
  }
}

