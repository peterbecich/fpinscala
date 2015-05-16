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

  def indexedSeqMonoid[B] = new Monoid[IndexedSeq[B]] {
    def op(a1: IndexedSeq[B], a2: IndexedSeq[B]) = a1 ++ a2
    val zero = IndexedSeq[B]()
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

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }


  def endoMonoid[B]: Monoid[B => B] = new Monoid[B => B] {
    // are a1 and a2 necessarily associative?

    /* note that monoids have no commutative property.
     Order of applications of b1 and b0 matters.
     */
    def op(bb0: B => B, bb1: B => B): B => B = (in: B) => bb1(bb0(in))

    // need type signature () => B
    //def zero: B =
    // not true ... A = (B => B)

    def zero: B => B = (in: B) => in
  }

  import fpinscala.testing._
  import Prop._
  import Gen._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val la0: Gen[List[A]] = Gen.listOfN(3, gen)
    val associative: Prop = Prop.forAll(la0){
      (la: List[A]) => {
        val a0 = la(0)
        val a1 = la(1)
        val a2 = la(2)
        val left = m.op(a0, m.op(a1, a2))
        val right = m.op(m.op(a0, a1), a2)
        left == right
      }
    }

    val rightIdentity: Prop = Prop.forAll(gen){
      (a: A) => {
        // shouldn't be necessary to test left because
        // associativity is tested above
        // ^^ that is commutivity, not associativity
        val right = m.op(a, m.zero)
        val left = m.op(m.zero, a)
        (right == a) && (left == a)
      }
    }

    val bothProperties: Prop = associative.&&(rightIdentity)

    bothProperties
  }


  //def trimMonoid(s: String): Monoid[String] = sys.error("todo")

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
    this._foldMapZ(as, m)(f)(m.zero)
  }
  // overloading not allowed in Scala because of potential
  // for currying.  Ambiguous whether currying or overloading.
  def _foldMapZ[A, B](as: List[A], m: Monoid[B])(f: A => B)(z: B): B = {
    // I think this is essentially an implementation of fold left...

    /*
     No issue with using List's built-in methods, as in the correct
     answer for par fold map.
     The pattern-matching solution above used List's built-in
     unapply method, so even that solution was dependent.
     */

    val bs: List[B] = as.map(f)
    // foldMapV assumes efficient indexing of sequence/vector
    // A List does not have efficient indexing.
    // So just implement this as a fold left

    @annotation.tailrec
    def aggregator(la: List[B], z: B): B = {
      /*
       Review fold left and fold right implementations in 
       fpinscala.datastructures.List, and
       why one is tail recursive and the other isn't
       */
      la match {
        case ((h: B):: Nil) => m.op(h, z)
        case ((h: B)::(t: List[B])) => aggregator(t, m.op(z, h))
      }
    }

    aggregator(bs, z)

  }


  /*
   Implemented by _foldMap; no circular dependency on other
   fold implementations.

   Assume that the below implementations of foldRight and 
   foldLeft do not have access to the list monoid --
   only _foldMap does.

   Where did this assumption come from?...
   */


  //lb.foldLeft(m.zero)(m.op)
  // def fold(list: List[B])(combiner: (B, List[B]) => B): B =
  //   list match {
  //     case Nil => m.zero
  //     case (head::Nil) =>
  //     case (head::tail) =>
  // }


  /*

   _foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B

   foldRight[C, D](as: List[C])(z: D)(f: (C, D) => D): D

   B == D
   A == C

   need monoid for D
   */
  // def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
  //   // {(A, B) => B} => {A => B => B}
  //   //val g: A =>(B => B) = (a: A) => ((b: B) => f(a,b))
  //   // trait
  //   // scala.Function1[A, Function1[B, B]]
  //   val g: A => (B => B) = f.curried
  //   // Thought that it was incorrect to hide type A => B => B in
  //   // type A => B...

  //   _foldMapZ(as, Monoid.endoMonoid)(g)((b: B) => z)

  //   /*
  //    as = List(65, 66, 67)
  //    z = ""
  //    f = (num, str) => num.toChar + str

  //    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B
  //    def _foldMapZ[C, D]
  //      (as: List[C], m: Monoid[D])(f: C => D)(z: D): D
  //    endoMonoid[E => E] {
  //      op (ee0: (E => E), ee1: (E => E)): (E => E) =
  //        (e) => ee1(ee0(e))
  //      zero: (E => E) = e => e
  //    }

  //    A == C and B=>B == D and B == E

  //    A = Int
  //    B = String


  //    def foldRight[Int, String]
  //      (as: List[Int])(z: String)(f: (Int, String) => String): String
  //    def _foldMapZ[Int, String]
  //      (as: List[Int], m: Monoid[String])(f: Int => String)
  //      (z: String): String
  //    endoMonoid[String => String] {
  //      op (ee0: (String => String), ee1: (String => String)):
  //        (String => String) =
  //        (e) => ee1(ee0(e))
  //      zero: (String => String) = e => e
  //    }


  //    expansion of _foldMapZ
  //    ___________________________________
  //    bs: List[String=>String]
     
     
     
     
  //    */

  // }


  def foldRight[A, B](la: List[A])(z: B)(f: (A, B) => B): B = {
    /*

    def endoMonoid[W]: Monoid[W => W] = 
      new Monoid[W => W] {
        def op(b0: W => W, b1: W => W): W => W = 
          (in: W) => b1(b0(in))
        def zero: W => W = (in: W) => in
      }

    def _foldMapZ[X, Y](as: List[X], m: Monoid[Y])(f: X => Y)(z: Y): Y
    W = B
    X = A
    Y = B
    as: List[A]
    endoMonoid: Monoid[C] = Monoid[B => B]
    f: A => B
    z: B
     */
    type C = B => B
    val g: A => C = f.curried

    // fpinscala.monoids.Monoid[Function1[B, B]]
    val endoMonoidC: Monoid[C] = endoMonoid[B] 
    /*
     def op(bb0: (B=>B), bb1: (B=>B)): B=>B
     def zero: B=>B
     */

    // how does this return B?
    // I would have expected C
    val b: B = foldMap(la, endoMonoidC){
      // need A => C
      g
    }{
      // need C starting value
      z
    }

    b

  }




  // tailrec annotation does not apply to calls inside this method;
  // tailrec annotation should be put on those methods called
  // @annotation.tailrec
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    // val bMonoid = new Monoid[List[B]] {
    //   def op(b0: B, b1: B)= b0 ++ b1
    //   def zero = Nil
    // }
    // _foldMap(as, bMonoid){
    //   // need A => B
    //   (a: A) => f(bMonoid.zero, a)
    // }
    //type C = A => B
    type C = B => B
    //val g: B => C = (b: B) => {(a: A) => f(b,a)}
    val g: A => C = (a: A) => {(b: B) => f(b,a)}

    // A=>B would not be an endofunction
    val endoMonoidC: Monoid[C] = endoMonoid[B] 

    // val b: B = _foldMapZ(as, dual(Monoid.endoMonoid)){
    //   // need A => C
    //   g
    // }{
    //   // need C starting value
    //   // B starting value...
    //   z
    // }
    val b: B = foldMap(as, dual(endoMonoidC)){
      // need A => C
      g
    }{
      // need C starting value
      // B starting value...
      z
    }

    b
  }

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
    // need to consider length == 0 case
    val b = if(length==0) m.zero
    else if (length==1) f(as.head)
    else {
      val approxHalf = length/2
      // right bound of slice is exclusive
      val leftRecursion: B = foldMapV(
        as.slice(0, approxHalf), m)(f)
      val rightRecursion: B = foldMapV(
        as.slice(approxHalf, length), m)(f)

      m.op(leftRecursion, rightRecursion)
    }
    b
  }: B

  // hint hint...
  import fpinscala.parallelism.Nonblocking._



  // Implement only for checking ordering of IndexedSeq[Int].
  // Could get more complicated to leave comparison abstract and check
  // ordering for IndexedSeq of any type

  /*
   While order checking will be associative, can be done in any order,
   that is not to say that the input IndexedSeq will be "scrambled".
   First and Following
   */

  def orderedIntMonoid =
    new Monoid[(Int, Int, Boolean)] {
      def op(
        first: (Int, Int, Boolean),
        following: (Int, Int, Boolean)
      ): (Int, Int, Boolean) = {
        val (min0, max0, ordered0) = first
        val (min1, max1, ordered1) = following

        //if ((ordered0 || ordered1) == false):
        val min2 = min0
        val max2 = max1

        val ordered2: Boolean =
          //if ((ordered0 || ordered1)==false) false
          if (ordered0 && ordered1 && (max0 < min1)) true
          else if (max0 > min1) false
          else true

        (min2, max2, ordered2)
      }

      /*
       May not be an identity for (Int, Int, Boolean)

       The only requirement is that 
       op(X, zero)=X, op(zero, X)=X, and op(zero, zero) = zero
       
       So there is nothing wrong with using "short circuits"
       to enforce this Identity.
       */
      def zero: (Int, Int, Boolean) = (0,0,true)
    }


  /*
   Two Par implementations
   fpinscala.parallelism.Par
   fpinscala.parallelism.Nonblocking.Par

   Not an implementation of Par
   fpinscala.parallelism.Actor
   */

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val aggregation = foldMapV(ints, orderedIntMonoid){
      // Int => (Int, Int, Boolean)
      (i: Int) => (i, i, true)
    }
    aggregation._3
  }


  sealed trait WC
  case class Stub(str: String) extends WC
  case class Part(lStr: String, words: Int, rStr: String) extends WC

  def parMonoid[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
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
    if(v.length==0){
      Par.unit(m.zero)
    } else if (v.length==1) {
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
    // don't use IndexedSeq's map or flatMap emethods
    //   (seqA: IndexedSeq[A]) => {
    // implicit def indexedSeqToList(is: IndexedSeq[A]): List[A] = is.toList

    // eventually figure out how to go use List in place of Sequence

    val parListB: Par[Seq[B]] = Par.parMap(v)(f)
    // Par[List[B]] => Par[B]
    // reduce in parallel
    val parB: Par[B] = Par.map(parListB){
      (listB: Seq[B]) => listB.foldLeft(m.zero)(m.op)
    }

    parB


  }
  /*
   we perform the mapping and the reducing both in parallel

   def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
   Par.parMap(v)(f).flatMap { bs =>
   note that foldMapV is not a parallelized function, but
   is parallelized by flatMap above.

   A single reduction cannot be parallelized, but multiple reductions
   can happen at the same time.

   val parB: Par[B] = foldMapV(bs, par(m))(b => Par.lazyUnit(b))

   parB
   }

   */



  /*
   Don't implemented WC monoid by turning each character into a stub
   */
  // val wcMonoid: Monoid[WC] = new Monoid[WC] {
  //   def op(wc0: WC, wc1: WC): WC = {

  //     //def (stub: Stub

  //     (wc0, wc1) match {
  //       case (
  //         Stub(str0), // left
  //         Stub(str1) // right
  //       ) if {
  //         str0.length()>0 &&
  //         str1.length()>0
  //       } == " "=> {
  //         Stub(str0 + str1)
  //       }
  //       case (
  //         Stub(str0), // left
  //         Stub(str1) // right
  //       ) if {
  //         str0.length()==0 &&
  //         str1.length()>0
  //       } == " "=> {
  //         Stub(str0 + str1)
  //       }
  //       case (
  //         Stub(str0),
  //         Part(lStub1, words1, rStub1)
  //       ) =>
  //         Part(str0+lStub1, words1, rStub1)
  //       case (
  //         Part(lStub0, words0, rStub0),
  //         Stub(str1)
  //       ) =>
  //         Part(lStub0, words0, rStub0+str1)
  //       case (
  //         Part(lStub0, words0, rStub0),
  //         Part(lStub1, words1, rStub1)
  //       ) => {
  //         // increment count and discard middle
  //         Part(lStub0, words0+words1+1, rStub1)
  //       }
  //     }
  //   }
  //   def zero: WC = Stub("")
  // }


  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(wc0: WC, wc1: WC): WC = {

      //def (stub: Stub

      (wc0, wc1) match {
        case (
          Stub(str0), // left
          Stub(str1) // right
        ) if {
          str0.length()>0 &&
          str1.length()>0
        } == " "=> {
          Stub(str0 + str1)
        }
        case (
          Stub(str0), // left
          Stub(str1) // right
        ) if {
          str0.length()==0 &&
          str1.length()>0
        } == " "=> {
          Stub(str0 + str1)
        }
        case (
          Stub(str0),
          Part(lStub1, words1, rStub1)
        ) =>
          Part(str0+lStub1, words1, rStub1)
        case (
          Part(lStub0, words0, rStub0),
          Stub(str1)
        ) =>
          Part(lStub0, words0, rStub0+str1)
        case (
          Part(lStub0, words0, rStub0),
          Part(lStub1, words1, rStub1)
        ) => {
          // increment count and discard middle
          Part(lStub0, words0+words1+1, rStub1)
        }
      }
    }
    def zero: WC = Stub("")
  }



  def count(s: String): Int = {
    val ac = s.toCharArray()
    val sc = ac.toIndexedSeq

    // map and reduce:
    // map each character to a WC

    //val sWc: IndexedSeq[WC] = ss.map((c: Char) => Stub(c.toString))
    // then reduce the WC
    // IndexedSeq[WC] => Int
    println("fold map v input: "+sc)
    //val wc: WC = foldMapV(ss, wcMonoid)((c: Char) => Stub(c.toString))

    //val sWc: IndexedSeq[WC] = sc.map((c: Char) => Stub(c.toString))

    /*
    note here that f returns Stub, a subtype of the type
    in the monoid passed to foldMap: WC.
    This should simplify when it's figured out how to do this 
     without turning each char into its own Stub -- there should
     be a better way...
     */

    val wc: WC = Monoid.foldMap(sc, Monoid.wcMonoid){
      (c: Char)=>Stub(c.toString)
    }
    println("fold map output: "+wc)

    val counted: Int = wc match {
      case Stub(_) => 0
      case Part(_, i, _) => i
    }
    counted
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(ab0: (A, B), ab1: (A, B)): (A, B) = {
        val a0 = ab0._1
        val a1 = ab1._1
        val a2 = A.op(a0, a1)
        val b0 = ab0._2
        val b1 = ab1._2
        val b2 = B.op(b0, b1)
        (a2, b2)
      }
      def zero: (A, B) = (A.zero, B.zero)
    }
    
  

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(f0: A=>B, f1: A=>B): A=>B = (a: A) => {
        val b0 = f0(a)
        val b1 = f1(a)
        val b2 = B.op(b0, b1)
        b2
      }
      def zero: A => B = (a: A) => B.zero    // ???
    }


  // def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
  //   sys.error("todo")

  // def bag[A](as: IndexedSeq[A]): Map[A, Int] =
  //   sys.error("todo")
}

object MonoidTest {
  import fpinscala.monoids.Monoid._
  import fpinscala.state.State
  import fpinscala.state.RNG
  import fpinscala.testing.Gen
  import fpinscala.testing.Prop


  val quickFox = "the quick brown fox jumps"

  val randWC: State.Rand[WC] = State{
    (rng: RNG) => {
      val (asciiCode: Int, nextRNG: RNG) = (RNG.chooseInt(rng)(65,90))
      val asciiChar: Char = asciiCode.toChar
      val asciiString: String = asciiChar.toString
      (Stub(asciiString), nextRNG)
    }
  }

  val genWC: Gen[WC] = Gen(randWC)

  def main(args: Array[String]): Unit = {
    /*
     Use int addition monoid and par monoid
     */
    val nums = (1 to 100).toList
    val sq = nums.toIndexedSeq
    //val sq = IndexedSeq(1 to 100)
    println("nums 1 to 100")
    val sum1: Int = foldMap(nums, intAddition)((i: Int) => i)
    println("sum with fold map: "+sum1)

    val ordered = Monoid.ordered(sq)
    println("ordered: "+ordered)
    val reverse = sq.reverse
    println("sequence reversed")

    val ordered2 = Monoid.ordered(reverse)
    println("ordered: "+ordered2)

    // val str = scala.util.Random.alphanumeric.take(40).
    // println("sentence: "+str)
    
    println("sentence: "+quickFox)
    val strWords = Monoid.count(quickFox)
    println("number of words: "+strWords)

    println("Testing that WC Monoid meets the Monoid laws")
    println("need Gen of WC")
    println(genWC)
    val wcMonoidProp: Prop = 
      Monoid.monoidLaws(Monoid.wcMonoid, genWC)

    println("wc monoid prop")
    println(wcMonoidProp)
    // val checked = wcMonoidProp.check
    // println(checked)

  }
}
trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero){
      (b: B, a: A) => {
        val b2: B = f(a)
        mb.op(b, b2): B
      }: B
    }: B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldMap(as)((a: A) => a)(m)

  def toList[A](as: F[A]): List[A] = {
    //type mismatch;
    // found   : fpinscala.monoids.Monoid[List[A]]
    // {val zero: scala.collection.immutable.Nil.type}
    // required: fpinscala.monoids.Monoid[A]

    //foldMap(as)((a: A) => a)(Monoid.listMonoid[A])
    foldLeft(as)(List[A]()){
      // (A, B) => B
      (la: List[A], a: A) => a :: la
    }
  }
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    // as match {
    //   case (h: A) :: Nil => f(h, z)
    //   case (h: A) :: (t: List[A]) => f(h, foldRight(t)(z)(f))
    // }
    as.foldRight(z)(f)

  // @annotation.tailrec
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    // as match {
    //   case (h: A) :: Nil => f(z, h)
    //   case (h: A) :: (t: List[A]) => foldLeft(t)(f(z, h))(f)
    // }
    as.foldLeft(z)(f)

  // @annotation.tailrec
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero){
      (b: B, a: A) => mb.op(b, f(a)): B
    }: B
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = {
    // val g: A => (B => B) = f.curried
    // Monoid.foldMapV(as, Monoid.indexedSeqMonoid)(g)
    as.foldRight(z)(f)
  }
    
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) = {
    // val bMonoid = new Monoid[B] {
    //   def op(b0: B, b1: B): B = 
    //   def zero: B = 
    // }
    // val g: A => B = (a: A) => f(z, a)

    // this.foldMap(as)(g)(bMonoid)

    as.foldLeft(z)(f)
    }

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = {
    // map and reduce -- and make use of IndexedSeq's efficient
    // index lookup.  List's lookup is not efficient.
    //val bs: IndexedSeq[B] = as.map(f)
    Monoid.foldMapV(as, mb)(f)

  }
    


}

// presumables Collections' Stream, not our own implementation
object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B]
    (as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(value) => f(value)
      case Branch(left, right) => {
        val leftB: B = foldMap(left)(f)(mb)
        val rightB: B = foldMap(right)(f)(mb)
        mb.op(leftB, rightB): B
      }
    }


  // s/Monoid.scala:753: could not optimize @tailrec annotated method foldLeft: it contains a recursive call not in tail position
  // [error]     as match {
  //@annotation.tailrec
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Leaf(value: A) => f(z, value)
      case Branch(left: Tree[A], right: Tree[A]) => {
        val leftB: B = foldLeft(left)(z)(f)
        val rightB: B = foldLeft(right)(leftB)(f)
        rightB
      }
    }

  //@annotation.tailrec
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {
    // as match {
    //   case Leaf(value: A) => f(z, value)
    //   case Branch(left: Tree[A], right: Tree[A]) => {
    //     val leftB: B = foldRight(left)(z)(f)
    //     val rightB: B = foldRight(right)(leftB)(f)
    //     rightB
    //   }
    // }
    val g: (B,A)=>B = (b: B, a: A) => f(a,b)
    val b: B = this.foldLeft(as)(z)(g)
    b
  }

}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Some(a) => f(a)
      case None => mb.zero
    }
  override def foldLeft[A, B]
    (as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Some(a) => f(z, a)
      case None => z
    }
    
  override def foldRight[A, B]
    (as: Option[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Some(a) => f(a, z)
      case None => z
    }
}

