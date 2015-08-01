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

  // How to do this without flatMap??  Functor has no flatMap.
  //def zip[A,B](fa: F[A], fb: F[B]): F[(A,B)]
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

   Primitive methods of applicative functor:
   unit and map2
   unit and apply

   */

  // unit and flatMap left abtract, as they were in Mon
  def unit[A](a: => A): M[A]
  // object unit {
  //   def unapply[A](ma: M[A]): A
  // }

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

  /*
   'product' is *not* 'zip'
   It is the Cartesion product
   */
  def product[A,B](ma: M[A], mb: M[B]): M[(A, B)] =
    this.map2(ma, mb)((a: A, b: B) => (a,b): Tuple2[A,B])


  // def zip[A,B](fa: M[A], fb: M[B]): M[(A,B)] = {
    /*
     M[A], M[B] => M[(A,B)]
     type is identical to 'product', but not the effect
     Want List(1,2,3,4,5).zip(List(a,b,c)) = List((1,a),(2,b),(3,c))
     Or Option(1).zip(Option("a")) = Option((1,"a"))
     */
    // this.flatMap(fa){(a: A) => {
    //   this.flatMap(fb){(b: B) => {
    //     this.unit((a,b))
    //   }
    //   }
    // }
    // }
    // ensures output list is of correct length (may be list of lists)
    // this.map(fa){(a: A) => {


    /*
     This is also the Cartesian product...
     

     this.flatMap(fa){(a: A) => {
     this.map(fb){(b: B) => {
     (a,b)
     }
     }
     }
     */
  // }

  /*
   Imagine the uses of filterM.
   ms = List(1,2,3,4)
   f: A => List[Boolean] = a > 2

   ms = List(1,2,3,4)
   f: A => Option[Boolean]   ???????

   */
  // def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {


  // }
  // exercise 12.11
  // show why not possible
  // def compose[G[_]](G: Monad[G]):
  //   Monad[({type f[x] = F[G[x]]})#f]
}



object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  // for Nonblocking Par
  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] =
    new Monad[P] {
      def unit[A](a: => A): P[A] = p.succeed(a)
      def flatMap[A,B](pa: P[A])(f: A=>P[B]): P[B] =
        p.flatMap(pa)(f)
    }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](op: Option[A])(f: A => Option[B]): 
        Option[B] = {
      op.flatMap(f)
    }

  }

  // Scala Collections' Stream, not fpinscala's laziness.Stream
  val collectionsStreamMonad: Monad[scala.collection.immutable.Stream] =
    new Monad[scala.collection.immutable.Stream] {
      def unit[A](a: => A): scala.collection.immutable.Stream[A] =
        scala.collection.immutable.Stream(a)
      def flatMap[A, B](st: scala.collection.immutable.Stream[A])
        (f: A => scala.collection.immutable.Stream[B]):
          scala.collection.immutable.Stream[B] = {
        st.flatMap(f)
      }
    }

  // our Stream implementation
  val streamMonad: Monad[fpinscala.laziness.Stream] =
    new Monad[fpinscala.laziness.Stream] {
      def unit[A](a: => A): fpinscala.laziness.Stream[A] =
        fpinscala.laziness.Stream.apply(a)
      def flatMap[A, B](st: fpinscala.laziness.Stream[A])(
        f: A => fpinscala.laziness.Stream[B]
      ): fpinscala.laziness.Stream[B] =
        st.flatMap(f)
    }

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
  //                 Monad[State[Int, A]]
  val intStateMonad = new Monad[IntState] {
    def unit[A](a: => A): IntState[A] = State.unit(a)
    def flatMap[A, B](st: IntState[A])(f: A => IntState[B]): 
        IntState[B] = {
      st.flatMap(f)
    }

    def getIntState[S]: State[S,S] = State.get
    def setIntState[S](s: => S): State[S, Unit] = State.set(s)


    /*
     This function numbers all the elements in a list using a State action. It keeps a state that’s an Int, which is incremented at each step. We run the whole composite state action starting from 0. We then reverse the result since we constructed it in reverse order.[10] 10 This is asymptotically faster than appending to the list in the loop. Note what’s going on with getState and setState in the for-comprehension. We’re obviously getting variable binding just like in the Id monad—we’re binding the value of each successive state action (getState, acc, and then setState) to variables. But there’s more going on, literally between the lines. At each line in the for-comprehension, the implementation of flatMap is making sure that the current state is available to getState, and that the new state gets propagated to all actions that follow a setState. What does the difference between the action of Id and the action of State tell us about monads in general? We can see that a chain of flatMap calls (or an equivalent for-comprehension) is like an imperative program with statements that assign to variables, and the monad specifies what occurs at statement boundaries. For example, with Id, nothing at all occurs except unwrapping and rewrapping in the Id constructor. With State, the most current state gets passed from one statement to the next. With the Option monad, a statement may return None and terminate the program. With the List monad, a statement may return many results, which causes statements that follow it to potentially run multiple times, once for each result. The Monad contract doesn’t specify what is happening between the lines, only that whatever is happening satisfies the laws of associativity and identity.


     */

    // def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    //   as.foldLeft(this.unit(List[(Int, A)]())){
    //     (acc: List[IntState[A]], a: A) => for {
    //       xs <- acc
    //       n <- getIntState
    //       _ <- setIntState(n+1)
    //     } yield (n, a) :: xs}.run(0)._1.reverse

  }

  // use Rand type declared in State
  val randStateMonad: Monad[State.Rand] = new Monad[State.Rand] {
    /*
     State.unit has parametric parameters S and A.  Since S is not
     made concrete as an argument to method, it is apparenly inferred to 
     be concretely RNG by the return type State.Rand[A] (==State[RNG, A])
     */
    def unit[A](a: => A): State.Rand[A] = State.unit(a)
    // type inference of State.Rand still works...
    // def unit[A](a: => A) = State.unit(a)
    def flatMap[A, B](rs: State.Rand[A])(f: A => State.Rand[B]):
        State.Rand[B] = {
      rs.flatMap(f)
    }
  }
  // why the funny parenthesis pattern,  ({...})   ?
  def intStateMonadAlt = 
    new Monad[({type IntStateAlt[A] = State[Int, A]})#IntStateAlt] {
    def unit[A](a: => A): State[Int, A] = State.unit(a)
    def flatMap[A, B](st: State[Int, A])(f: A => State[Int, B]): 
        State[Int, B] = {
      st.flatMap(f)
    }
  }

  // from answers...
  // Since `State` is a binary type constructor, we need to partially apply it
  // with the `S` type argument. Thus, it is not just one monad, but an entire
  // family of monads, one for each type `S`. One solution is to create a class
  // `StateMonads` that accepts the `S` type argument and then has a _type member_
  // for the fully applied `State[S, A]` type inside:
  class StateMonads[S] {
    type StateS[A] = State[S, A]
    // We can then declare the monad for the `StateS` type constructor:
    val monad = new Monad[StateS] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))
      override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }
  }


  // S replaced by Int
  def stateMonad[S] =  // what is the type of this?
    new Monad[({type f[X] = State[S,X]})#f] {
      //             ^ an anonymous type: fpinscala.state.State[S, X]
      def unit[A](a: => A): State[S,A] = State((s: S) => (a,s))
      def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] =
        st.flatMap(f)
    }

  // def intStateMonadAlt2[A]: Monad[Int] = stateMonad[Int]
  //                       ^ not Monad[State[(Int,A)]]

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


object MonadTest {
  
  def genId[A](aGen: Gen[A]): Gen[Id[A]] = aGen.map(Id(_))

  def genIdInt: Gen[Id[Int]] = {
    // confusion in State.scala is two uses of type name 'Rand'
    // they seem to be interoperable
    val stateInt: State.Rand[Int] = State(RNG.randInt)
    val genInt: Gen[Int] = Gen(stateInt)
    genId(genInt)
  }
  def genListIdInt(n: Int): Gen[List[Id[Int]]] = Gen.listOfN(n, genIdInt)

  // def idProps: Prop = {
  //   val associativity: Prop = 
  //     Prop.forAll(genListIdInt(3)){(ll: List[Id[Int]]) => {
  //       val idInt0: Id[Int] = ll(0)
  //       val idInt1 = ll(1)
  //       val idInt2 = ll(2)
  //       //  m flatMap f flatMap g == m flatMap (x => f(x) flatMap g)
  //       //val left = idInt0.flatMap(int0 => 

  //       val right = idInt0.flatMap{(int0: Int) => {
  //         val int1: Int = 

  val zipIntMonad = Monad.stateMonad[Int]
  val zipIntMonads = new Monad.StateMonads[Int]
  val mnd = zipIntMonads.monad
  /*  ^^^^
   Can't seem to get type of this out of Ensime.
   Would have thought type is Monad[State[Int,A]]
   where def is 'def mnd[A]'.
   vals can't make parametric type -- val foo[A] doesn't work.
   class
   fpinscala.monads.Monad$$StateMonads$<refinement>
   */


  // def counter(start: Int, n: Int): List[Int] = {
  //   val counterMonad = Monad.stateMonad[(Int, List[Int])]
  //   // State[S=(head: Int, list: List[Int]), A=list: List[Int]]
  //   // State should not contain its remaining number of increments
  //   val blankState: State[(Int, List[Int]), List[Int]] =
  //     State((s: (Int, List[Int])) => {
  //       val nextListHead = s._1 + 1
  //       val nextList = nextListHead::s._2
  //       (nextList, (nextListHead,nextList))
  //     }
  //     )

    // def unit[A](a: => A): State[S,A] = State((s: S) => (a,s))
    // val startState: State[(Int, List[Int]), Int] =
    //   counterMonad.unit(start)
    // def replicateM[A](n: Int, ma: M[A]): M[List[A]]
    // def replicateM[List[Int]](n: Int, ma: State[List[Int]]):
    //   State[List[Int]]
    // val listState:  = counterMonad.replicateM(n, blankState)
    // val ran: ((Int, List[Int]), =
    //   stateList.run((start, List[Int]()))
    // ran._2

    // for {
    //   i <- 0 to n;
      

  // }

  def zipWithIndex[A](as: List[A]): List[(Int,A)] = {
    val ran: (List[(Int, A)], Int) = zipWithIndexState(as).run(0)
    val nextState: Int = ran._2
    println("next state: "+nextState)
    val finalListTuple: List[(Int, A)] = ran._1
    println("run state output: "+finalListTuple)
    finalListTuple.reverse
  }

  // listing 11.8 with for comprehension made explicit
  def zipWithIndexState[A](as: List[A]): State[Int, List[(Int,A)]] = {

    // as.foldLeft(Monad.stateMonad[Int].unit(List[(Int, A)]())){(acc,a)=>
    //   for {
    //     xs <- acc
    //     n <- State.get
    //     _ <- State.set(n+1)
    //  } yield (n, a) :: xs }
    as.foldLeft(Monad.stateMonad[Int].unit(List[(Int, A)]())){
      (acc,a) =>
      acc.flatMap(xs =>
        State.get.flatMap(n =>
          State.set(n+1).flatMap(_ =>
            State.unit((n,a)::xs)
          )
        )
      )
    }

    // .run(0)._1.reverse
    /*
     Translating for-comprehensions

     Scala’s “for comprehensions” are syntactic sugar for composition of multiple operations with foreach, map, flatMap, filter or withFilter. Scala actually translates a for-expression into calls to those methods, so any class providing them, or a subset of them, can be used with for comprehensions.

     http://docs.scala-lang.org/tutorials/FAQ/yield.html
     */

    //fpinscala.state.State[Int, List[Tuple2[Int, A]]]
    // state's S = Int, state's A = List[(Int, A)]
    // val emptyZip: State[Int, List[(Int,A)]] =
    //   zipIntMonad.unit(List[(Int,A)]())

    // val f: (State[Int, List[(Int,A)]], A) => State[Int, List[(Int,A)]] = 
    //   (acc: State[Int, List[(Int,A)]], a: A) => {
    //     val nextState: State[Int, Unit] =
    //       acc.flatMap{(xs: List[(Int,A)]) => {
    //         val getter: State[Int, Int] = State.get
    //         val newState: State[Int, List[(Int,A)]] =
    //           getter.flatMap{(n:Int) => {
    //             val setter: State[Int, Unit] = State.set(n+1)
    //             setter.map(_=>(n, a)::xs)
    //           }
    //           }
    //         newState
    //       }
    //       }
    //   }

    // val aggregatedState: State[Int, List[(Int,A)]] =
    //   as.foldLeft(emptyZip)(f)
    // aggregatedState
  }

  def main(args: Array[String]): Unit = {
    // val ll = (10 to 20).toList
    // val ll2 = (10 to 20).toList
    val ll = List(1,2,3,4,5,6)
    val ll2 = List(4,5,6,7,8,9)
    val oi: Option[Int] = Some(4)
    val os: Option[String] = Some("foobar")
    val mapped = Monad.listMonad.map(ll)((i: Int) => i + 1)
    println(ll)
    println(mapped)
    println("product")
    val llProduct = Monad.listMonad.product(ll, ll2)
    println(llProduct)
    // println("zipped")
    // println(Monad.listMonad.zip(ll,ll2))
    // println(Monad.optionMonad.zip(oi,os))


    println("---------------------------")
    val F = Monad.intStateMonad
        
    println("---------------------------")
    println("use of Id and Id monad")
    // We could say that monads provide a context for introducing and binding variables, and performing variable substitution.

    val hello = Id("Hello ")
    val there = Id("there ")
    val mnd = Id("monad!")
    val wordList: List[Id[String]] = List(hello,there,mnd)
    val sentence: Id[String] = hello.flatMap(str0 => 
      there.flatMap(str1 =>
        mnd.flatMap(str2 =>
          Id(str0+str1+str2)
        )
      )
    )
    println(sentence)
    println("using sequence")
    println(Monad.idMonad.sequence(wordList))
    println("using replicate")
    println(Monad.idMonad.replicateM(5,hello))
    println("using product")
    println(Monad.idMonad.product(hello,mnd))

    println("---------------------------")
    // println("use of Int State monad")
    // val one = State.unit(1)
    // println("state of one: "+one)

    println("---------------------------")
    println("use of zipWithIndex")
    val letters = (65 to 91).map(_.toChar).toList
    println("on letters: "+letters)
    val zipped = MonadTest.zipWithIndex(letters)
    println("zipped:")
    for(tpl <- zipped) println(tpl)


    // println("---------------------------")
    // println("use of counter, from 5, for 9")
    // println(counter(5,9))


  }
}

/*
 A monad trait based on the second primitive set of methods
 to be left abstract: unit and compose
 */
trait Monad2[M[_]] extends Monad[M] {
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
trait Monad3[M[_]] extends Monad[M] {
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


