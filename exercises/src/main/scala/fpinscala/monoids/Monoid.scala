package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc

import scala.language.higherKinds
import scala.language.implicitConversions

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


   // Implement only for checking ordering of IndexedSeq[Int].
   // Could get more complicated to leave comparison abstract and check
   // ordering for IndexedSeq of any type


  def orderedIntMonoid = 
    new Monoid[(Int, Int, Boolean)] {
    def op(
      first: (Int, Int, Boolean), 
      following: (Int, Int, Boolean)
    ): (Int, Int, Boolean) = {

    }

    def zero: Boolean = true
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

  def concatenate[A](as: List[A], m: Monoid[A]): A = {
    as.foldLeft(m.zero)(m.op)
  }
  /*
   But what if our list has an element type that doesn’t have a Monoid instance? Well, we can always map over the list to turn it into a type that does:
   */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    /*
     Monoid[B] {
       def op(b1: B, b2: B): B
       def zero: B
     }
     */
    val lb = as.map(f)
    lb.foldLeft(m.zero)(m.op)
  }

  // foldMap with no dependency on other fold implementations
  def _foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    /*
     Monoid[B] {
       def op(b1: B, b2: B): B
       def zero: B
     }
     */

    // I think this is essentially an implementation of fold right...
    as match {
      case Nil => m.zero
      case (a :: Nil) => {
        val b = f(a)
        b
      }
      case (a :: tail) => {
        val b = f(a)
        val b2 = m.op(b, _foldMap(tail, m)(f))
        b2
      }
    }

  }


  /*
   Implemented by _foldMap; no circular dependency on other
   fold implementations.

   Assume that the below implementations of foldRight and 
   foldLeft do not have access to the list monoid --
   only _foldMap does.
   */


    //lb.foldLeft(m.zero)(m.op)
    // def fold(list: List[B])(combiner: (B, List[B]) => B): B =
    //   list match {
    //     case Nil => m.zero
    //     case (head::Nil) => 
    //     case (head::tail) =>
    // }


  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  /*
   Reduces the number of strings allocated and deallocated.
   Clearly not parallelized, though.

   In some cases, this function will run out of memory
   where foldLeft would not.  foldLeft is tail recursive.

   Old question answered: even a tail-recursive function
   can run out of memory.  For example,
   the output of the fold could be enormous.
   */
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val length = as.length
    val approxHalf = length/2
    // right bound of slice is exclusive
    val leftRecursion: B = foldMapV(
      as.slice(0, approxHalf), m)(f)
    val rightRecursion: B = foldMapV(
      as.slice(approxHalf, length), m)(f)

    m.op(leftRecursion, rightRecursion)

  }

  // hint hint...
  import fpinscala.parallelism.Nonblocking._

  /*
   Two Par implementations
   fpinscala.parallelism.Par
   fpinscala.parallelism.Nonblocking.Par

   Not an implementation of Par
   fpinscala.parallelism.Actor
   */

  def ordered(ints: IndexedSeq[Int]): Boolean = {

  }


  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    //  type Par[A] = ExecutorService => Future[A]
    def op(par1: Par[A], par2: Par[A]): Par[A] = {
      // This does too much... runs the two Pars
      // (es: java.util.concurrent.ExecutorService) => {
      //   // remember you have an op to combine two A values
      //   val par3 = Par.map2(par1, par2)(m.op): Par[A]
      //   par3.run(es)
      // }
      Par.map2(par1, par2)(m.op): Par[A]
    }
    def zero: Par[A] = Par.unit(m.zero)
  }

  // split down the middle and merge
  def _parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B):
      Par[B] =
    if(v.length==1){
      val head: A = v.head   // I don't know how this is typesafe
                             // for an empty Seq
      val b: B = f(head)
      val parB: Par[B] = Par.delay(b)

      parB
    } else {
      val middle = v.length / 2
      val (leftSeq, rightSeq):
          Tuple2[IndexedSeq[A], IndexedSeq[A]] =
        v.splitAt(middle)

      val parLeft: Par[B] = parFoldMap(leftSeq, m)(f)
      val parRight: Par[B] = parFoldMap(rightSeq, m)(f)

      val parMerged: Par[B] = Par.map2(parLeft, parRight)(m.op)

      parMerged
    }

  // improved
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B):
      Par[B] = {
    /*
     Think of this as
     Par[IndexedSeq[A]] => Par[B]
     */
    //val parSeqA: Par[IndexedSeq[A]] = Par.delay(v)
    // Par.flatMap(parSeqA){
    //   // don't use IndexedSeq's map or flatMap emethods
    //   (seqA: IndexedSeq[A]) => {
    implicit def indexedSeqToList(is: IndexedSeq[A]): List[A] = is.toList

    val parListB: Par[List[B]] = Par.parMap(v)(f)
    // Par[List[B]] => Par[B]
    // reduce in parallel
    val parB: Par[B] = Par.map(parListB){
      (listB: List[B]) => listB.foldLeft(m.zero)(m.op)
    }

    parB


  }





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

object MonoidTest {
  import fpinscala.monoids.Monoid._


  def main(args: Array[String]): Unit = {
    /*
     Use int addition monoid and par monoid
     */
    val nums = (1 to 100).toList
    println("nums 1 to 100")
    val sum1: Int = foldMap(nums, intAddition)((i: Int) => i)
    println("sum with fold map: "+sum1)


  }
}
trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = ???
  // {
  //   as.


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
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = 
    as match {
      case Leaf(value: A) => f(z, value)
      case Branch(left: Tree[A], right: Tree[A]) => {
        val leftB: B = foldLeft(left)(z)(f)
        val rightB: B = foldLeft(right)(leftB)(f)
        rightB
      }
    }
  // override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
  //   as match {
  //     case Leaf(value: A) => f
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

