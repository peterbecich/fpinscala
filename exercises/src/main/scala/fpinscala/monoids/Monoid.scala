package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[B] = new Monoid[List[B]] {
    def op(a1: List[B], a2: List[B]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    val zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 | a2
    val zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 & a2
    val zero: Boolean = true
  }

  /*
   scala.Option, not fpinscala...option
   
   Combine two options of unknown type with Flatmap, or Map...
   Each Option must define these methods
   */


/*
class
scala.Option[A] doc source
---------------------------
class		    WithFilter doc
<init>		    () => Option[A]
collect		    (PartialFunction[A, B]) => Option[B]
contains	    (A1) => Boolean
exists		    (Function1[A, Boolean]) => Boolean
filter		    (Function1[A, Boolean]) => Option[A]
filterNot	    (Function1[A, Boolean]) => Option[A]
flatMap		    (Function1[A, Option[B]]) => Option[B]
flatten		    (<:<[A, Option[B]]) => Option[B]
fold		    (<byname>[B]) => (Function1[A, B]) => B
forall		    (Function1[A, Boolean]) => Boolean
foreach		    (Function1[A, U]) => Unit
get		    A
getOrElse	    (<byname>[B]) => B
isDefined	    Boolean
isEmpty		    Boolean
iterator	    Iterator[A]
map		    (Function1[A, B]) => Option[B]
nonEmpty	    Boolean
orElse		    (<byname>[Option[B]]) => Option[B]
orNull		    (<:<[Null, A1]) => A1
toLeft		    (<byname>[X]) => <refinement>
toList		    List[A]
toRight		    (<byname>[X]) => <refinement>
withFilter	    (Function1[A, Boolean]) => WithFilter
 */
  def optionMonoid[B]: Monoid[Option[B]] = new Monoid[Option[B]] {
    def op(a1: Option[B], a2: Option[B]): Option[B] = {
      //a1.flatMap(a2)
      a1.orElse(a2)
    }
    val zero: Option[B] = None
  }

  def endoMonoid[B]: Monoid[B => B] = new Monoid[B => B] {
    // are a1 and a2 necessarily associative?
    def op(a1: B => B, a2: B => B): B => B = (in: B) => a1(a2(in))


    // need type signature () => B
    //def zero: B = 
    // not true ... A = (B => B)

    def zero: B => B = (in: B) => in
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = new Prop {

  }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    sys.error("todo")

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = ??? //as match {
//    case h::t



  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    sys.error("todo")

  def ordered(ints: IndexedSeq[Int]): Boolean =
    sys.error("todo")

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    sys.error("todo")

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    sys.error("todo") 

  val wcMonoid: Monoid[WC] = sys.error("todo")

  def count(s: String): Int = sys.error("todo")

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

