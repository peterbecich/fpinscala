package fpinscala
package applicative

import monads.Functor
import state._
import State._
//import StateUtil._ // defined at bottom of this file
import monoids._
import scala.{Stream => _}
import laziness.Stream
import scala.language.higherKinds
import scala.language.implicitConversions


trait Applicative[F[_]] extends Functor[F] {

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  // exercise 12.2
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

  // not much of a point in re-doing this with our List implementation
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

  /*
   Product of applicative functors of same subtype.
   i.e. product of Option[Int] and Stream[Int] = 
        (Option[Int], Stream[Int])
        that looks too simple...
   */
  def product[G[_]](G: Applicative[G]):
      Applicative2[({type f[x] = (F[x], G[x])})#f] = {
    val applicativeF: Applicative[F] = this

    new Applicative2[({type f[x] = (F[x], G[x])})#f]{
      // implement primitives unit and apply
      override def unit[A](a: => A): (F[A], G[A]) =
        (applicativeF.unit(a), G.unit(a))

      // apply is easier than map2, in this case
      override def apply[A,B](fabgab: (F[A => B], G[A => B]))(
        faga: (F[A], G[A])):  (F[B], G[B]) = {
        val fb: F[B] = applicativeF.apply(fabgab._1)(faga._1)
        val gb: G[B] = G.apply(fabgab._2)(faga._2)
        (fb,gb)
        }
    }
  }

  def compose[G[_]](G: Applicative[G]):
      Applicative2[({type f[x] = F[G[x]]})#f] = {
    val applicativeF: Applicative[F] = this

    new Applicative2[({type f[x] = F[G[x]]})#f]{
      override def unit[A](a: => A): F[G[A]] = {
        val ga: G[A] = G.unit(a)
        val fga: F[G[A]] = applicativeF.unit(ga)
        fga
      }
      override def apply[A,B](fgab: F[G[A=>B]])(fga: F[G[A]]): F[G[B]] = {
        applicativeF.map2(fgab, fga){(gab: G[A=>B], ga: G[A]) => {
          G.apply(gab)(ga)
        }
        }
      }
    }
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = ???
}

// exercise 12.2
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


/*
 A monad with the same set of primitives as
 Monad3: unit, map, and join.
 unit and map are sent to Applicative.
 join is left abstract.
 */
trait Monad4[F[_]] extends Applicative[F] {
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

  // override Applicative's abstract primitive map2
  override def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    this.flatMap(fa){(a:A)=>{
      this.map(fb){(b:B)=>{
        f(a,b)
      }
      }
    }
    }

  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)
}

object Monad4 {
  /*
   Either cannot contain multiple errors.
   This exercises points out both the chain of dependencies of
   monad application,
   and also the insufficiency of Either.
   */
  def eitherMonad[E]: Monad4[({type f[x] = Either[E, x]})#f] =
    new Monad4[({type f[x] = Either[E, x]})#f]{
      // implement unit, map2 and join
      def unit[A](a: => A): Either[E,A] = scala.util.Right(a)
      override def map2[A,B,C](ea: Either[E,A], eb: Either[E,B])(f: (A,B)=>C):
          Either[E,C] = (ea, eb) match {
        case (Right(a), Right(b)) => Right(f(a,b))
        case (Left(e1), _) => Left(e1)
        case (_, Left(e2)) => Left(e2)
      }
    }

  def stateMonad[S] = new Monad4[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad4[F], N: Monad4[N], T: Traverse[N]):
    Monad4[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
  extends Validation[E, Nothing]        // default ^^ was missing

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  // Collections' Stream
  val collectionsStreamApplicative = new Applicative[scala.Stream] {

    def unit[A](a: => A): scala.Stream[A] =
      scala.Stream.continually(a) // The infinite, constant stream

    def map2[A,B,C](a: scala.Stream[A], b: scala.Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): scala.Stream[C] =
      a zip b map f.tupled
  }
  // fpinscala Stream
  val streamApplicative = new Applicative[fpinscala.laziness.Stream] {
    def unit[A](a: => A): Stream[A] = Stream._constant(a)
    def map2[A,B,C](sa: Stream[A], sb: Stream[B])(f: (A,B)=>C): Stream[C] =
      sa.map2(sb)(f)

    /*
     Exercise 12.4
     The meaning of
     def sequence[A](fas: List[Stream[A]]): Stream[List[A]]

     It is not
      concatenating Streams into one Stream
      multiplexing Streams into one Stream
     It is
      forming a Stream of Lists of elements of the same type 
      from elements in provided Streams at equal indices

     sequence(List(numbers, numbers)) = 
     Stream((1,1),(2,2),...)
     */

  }

  def validationApplicative[E]:
      Applicative[({type f[x] = Validation[E,x]})#f] =
    new Applicative[({type f[x] = Validation[E,x]})#f] {
      // implement unit and map2
      // (or unit and apply)
      def unit[A](a: => A): Validation[E,A] = Success(a)
      def map2[A,B,C](va: Validation[E,A], vb: Validation[E,B])(
        f: (A,B) => C): Validation[E,C] = (va, vb) match {
        case (Success(a), Success(b)) => Success(f(a,b))
        case (Success(a), Failure(e2, vecE2)) => Failure(e2, vecE2)
        case (Failure(e1, vecE1), Success(b)) => Failure(e1, vecE1)
        // Assuming error in head not stored in tail
        case (Failure(e1, vecE1), Failure(e2, vecE2)) =>
          Failure(e1, vecE1 ++ Vector[E](e2) ++ vecE2)
      }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative2[({ type f[x] = Const[M, x] })#f] {
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
  val idMonad = new Monad4[Id] {
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
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad4.stateMonad)

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

object ApplicativeTests {
  val listStreamInt: List[Stream[Int]] =
    List(Stream._from(1), Stream._from(4), Stream._fibs)
  val sequencedStreams: Stream[List[Int]] =
    Applicative.streamApplicative.sequence(listStreamInt)

  // web form example -- Listing 12.5
  case class WebForm(
    name: String,
    birthdate: java.util.Date,
    phoneNumber: String
  )
  def validName(name: String): Validation[String, String] =
    if(name != "") Success(name)
    else Failure("Name cannot be empty")
  def validBirthdate(birthdate: String): Validation[String, java.util.Date] =
    try {
      import java.text._
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      case e => Failure("Birthdate must be in form yyyy-MM-dd")
        // ^^ case statement was neccessary
        // http://stackoverflow.com/questions/19950345/value-isdefinedat-is-not-a-member-of-play-api-mvc-simpleresult
    }
  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}")) // << learn this regex
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

  // "lift" with map3
  def validWebForm(name: String, birthdate: String, phone: String):
      Validation[String, WebForm] =
    Applicative.validationApplicative.map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone)
    ){(  // << interesting difference between {} and () shown here
         // () required to enclosure multiple arguments,
         // but not one argument?
      successfulName: String,
      successfulBirthdate: java.util.Date,
      successfulPhone: String
    ) => WebForm(successfulName, successfulBirthdate, successfulPhone)
    }

  val badName = ""
  val badBirthdate = "1000 AD"
  val badPhone = "411"

  val validatedWebForm: Validation[String, WebForm] =
    validWebForm(badName, badBirthdate, badPhone)

  def main(args: Array[String]): Unit = {
    println("Stream of Lists of Ints")
    for(l<-sequencedStreams.toListFinite(10)) println(l)

    println("web form example -- Listing 12.5")
    validatedWebForm match {
      case Success(form) => println(form)
      case Failure(h, tail) => {
        println("first error: "+h)
        println("others: "+tail)
      }
    }

  }
}

