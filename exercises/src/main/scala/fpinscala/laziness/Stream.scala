package fpinscala.laziness

//import Stream._
import fpinscala.monads.Monad
import fpinscala.monads.Functor

import scala.collection.immutable.{Stream => _}


trait Stream[+A] {


  //foo
  //bar

  def foldRight[C, B<:C](z: => B)(f: (A, => B) => B): C = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Stream.cons(h,t) => f(h, t.foldRight(z)(f))
      // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  /*
   Think about implementing fold left over a Stream.
   I think the same downside of List's fold left occurs here -- reversing.
   But you can't reverse an infinite stream...
   */
  // def foldLeft[B](z: => B)(f: (=> B, A) => B): B =
  //   this match {
  //     case Cons(h,t) => t().foldLeft(f(z,h()))(f)
  //     case _ => z
  //   }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.


  // {
  //   def g(a: A, oa: Option[A]): Option[A] = oa match {
  //     case Some(a) => Some(a)
  //     case None => if(f(a)==true) Some(a) else None
  //   }
  //   foldRight()(g)
  // }

  /*
   Tail is being evaluated strictly, leading to endless loop,
   I think
   */
  //@annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Stream.cons(h, t) => {
      // println("h: "+h)
      val found: Option[A] = if(f(h)==true) Some(h) else t.find(f)
      found
    }
    case _ => None
  }

  /*
   The purpose of 'take' is to insert a Stream.empty at the nth node.
   */
  def take(n: Int): fpinscala.laziness.Stream[A] = this match {
    // case Empty => fpinscala.laziness.Stream.empty[A]
    // case Cons(h, t) => Cons(h, ()=>take(n-1)) // is laziness preserved 
    //  here?
    case fpinscala.laziness.Stream.cons(h, t) if n>0 =>
      fpinscala.laziness.Stream.cons(h, t.take(n-1))
    case _ if n==0 => Stream.empty
    case _ => Stream.empty
  }
  def drop(n: Int): fpinscala.laziness.Stream[A] = this match {
    case fpinscala.laziness.Stream.cons(h, t) if n>0 =>
      t.drop(n-1)
    case fpinscala.laziness.Stream.cons(h, t) if n==0 =>
      fpinscala.laziness.Stream.cons(h, t)
    case _ => fpinscala.laziness.Stream.empty
  }

  def headOption: Option[A] = find((a: A) => true)
//  def _headOption: Option[A] = foldRight

  // could easily run forever


  /*
   Understandable...
   scala> fpinscala.laziness.Stream.from(4).toList
   java.lang.StackOverflowError
   at fpinscala.laziness.Stream$$anonfun$from$1.apply(Stream.scala:260)
   at fpinscala.laziness.Stream$$anonfun$from$1.apply(Stream.scala:260)
   at fpinscala.laziness.Stream$.head$lzycompute$1(Stream.scala:220)
   at fpinscala.laziness.Stream$.fpinscala$laziness$Stream$$head$1(Stream.scala:220)
   at fpinscala.laziness.Stream$$anonfun$cons$1.apply(Stream.scala:222)
   at fpinscala.laziness.Stream$class.toList(Stream.scala:49)
   at fpinscala.laziness.Cons.toList(Stream.scala:216)
   at fpinscala.laziness.Stream$class.toList(Stream.scala:49)
   */
  def toList: List[A] = this match {
    //case Stream.empty => List[A]()
    case Stream.cons(h, t) => h :: t.toList
    case _ => List[A]()
  }
  def toListFinite(n: Int): List[A] = {
    // def f(a: A, la: => List[A]): List[A] = a::la
    // foldRight(List[A]())(f)
    this match {
      //case Empty => List[A]()
      case Stream.cons(h, t) if n>0 => h :: t.toListFinite(n-1)
      case _ => List[A]()
    }
  }
  def feedback: Unit = 
    println(this.toListFinite(150))


  // def drop(n: Int): fpinscala.laziness.Stream[A] = this match {
  //   case Empty => empty[A]
  //   case Cons(h, t) if n>0 => t().drop(n-1)
  //   case Cons(h, t) => t()
  // }



  def forAll(p: A => Boolean): Boolean = foldRight(true)(
    (a, b) => p(a) && b
  )

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.


  // use fold right
  // foldRight(=>fpinscala.laziness.Stream[B])((A, =>fpinscala.laziness.Stream[B])=>fpinscala.laziness.Stream[B])

  def map[B](f: A => B): fpinscala.laziness.Stream[B] = {
    def g(a: A, sb: => fpinscala.laziness.Stream[B]):
        fpinscala.laziness.Stream[B] =
      fpinscala.laziness.Stream.cons(f(a), sb)
    //                                     ^
    // Big mistake to call the next iteration of 'map' here.
    // That shows a misunderstanding of the use of 'fold', left or right

    foldRight(Stream.empty[B])(g)
  }

  // Know why this does not 'zip' implicitly.
  // Probably the same reason that Monad's 'product' cannot
  // implement 'zip'.
  // def map2[B,C](sb: Stream[B])(f: (A,B)=>C): Stream[C] =
  //   this.flatMap{(a:A)=>{
  //     sb.map{(b:B)=>{
  //       f(a,b)
  //     }
  //     }
  //   }
  //   }
  def map2[B,C](sb: Stream[B])(f: (A,B)=>C): Stream[C] = {
    val sab: Stream[(A,B)] = this.zip(sb)
    //val merge: Tuple2[A,B] => C = ((a,b))=>f(a,b)
    def merge(tpl: Tuple2[A,B]): C = f(tpl._1, tpl._2)
    val sc: Stream[C] = sab.map(merge)
    sc
  }

  /*
   Learn about covariance, invariance and contravariance.
   An upper type bound T <: A declares that type variable T refers to a subtype of type A. 

   With fpinscala.laziness.Stream[A]:

   pattern type is incompatible with expected type;
   found   : fpinscala.laziness.Empty.type
   required: fpinscala.laziness.Stream[A]
   Note: Nothing <: A 
   (and fpinscala.laziness.Empty.type <:
   fpinscala.laziness.Stream[Nothing]), 
   but trait fpinscala.laziness.Stream is invariant in type A.
   You may wish to define A as +A instead. (SLS 4.5)

   With fpinscala.laziness.Stream[+A]:

   covariant type A occurs in contravariant position in type fpinscala.laziness.Stream[A] of value stream2
   def append(stream2: fpinscala.laziness.Stream[A]): fpinscala.laziness.Stream[A] = {
   ^
   stream2 being contravariant means:
   Given
   A is a supertype of B  (Number is a supertype of Double)
   A >: B

   fpinscala.laziness.Stream[A] is a supertype of fpinscala.laziness.Stream[B] 
   (fpinscala.laziness.Stream[Number] supertype of fpinscala.laziness.Stream[Double])
   fpinscala.laziness.Stream[A] >: fpinscala.laziness.Stream[B]

   An error is forcing stream2 to be contravariant
   stream2: fpinscala.laziness.Stream[A] is a ***subtype*** of 
   stream2: fpinscala.laziness.Stream[B]
   (stream2: fpinscala.laziness.Stream[Number] is a subtype of stream2: fpinscala.laziness.Stream[Double])

   stream2: fpinscala.laziness.Stream[A] <: stream2: fpinscala.laziness.Stream[B]
   
   */
  //def append(stream2: Stream[A]): Stream[A]
  def append[B >: A](stream2: fpinscala.laziness.Stream[B]):
      fpinscala.laziness.Stream[B] = {
    // type
    // (fpinscala.laziness.A,
    // scala.<byname>[fpinscala.laziness.Stream[A]]) =>
    // fpinscala.laziness.Stream[A]
    def f(a: A, sa: => fpinscala.laziness.Stream[B]):
        fpinscala.laziness.Stream[B] = 
      fpinscala.laziness.Stream.cons(a, sa)

    /*
    how is a lazy argument specified in an anonymous function?
    val f: (A, => fpinscala.laziness.Stream[A]) => fpinscala.laziness.Stream[A] = 
      (a: A, sa: => fpinscala.laziness.Stream[A]) => fpinscala.laziness.Stream.cons(a, sa)
    val f: (A, fpinscala.laziness.Stream[A]) => fpinscala.laziness.Stream[A] = 
      (a: A, sa: fpinscala.laziness.Stream[A]) => fpinscala.laziness.Stream.cons(a, sa)

    regarding foldRight below
    type
    (scala.<byname>[B]) =>
    (scala.Function2[A, <byname>[B], B]) =>
    fpinscala.laziness.B

    filled in with actual types
    type
    (scala.<byname>[fpinscala.laziness.Stream[A]]) =>
    (scala.Function2[A, <byname>[fpinscala.laziness.Stream[A]], fpinscala.laziness.Stream[A]]) =>
    fpinscala.laziness.Stream[A]
     */
    val out: fpinscala.laziness.Stream[B] = foldRight(stream2)(f)
    out
  }
  def flatMap[B](f: A => fpinscala.laziness.Stream[B]): fpinscala.laziness.Stream[B] = {
    def g(a: A, sb: => fpinscala.laziness.Stream[B]) = f(a).append(sb)
    foldRight(Stream.empty[B])(g)
  }


  /*
   covariant type A occurs in contravariant position in type fpinscala.laziness.Stream[A] of value sa2

   Meaning...
   Given Number >: Double
   Stream[Number] >: Stream[Double]
   but
   sa2: Stream[Number] <: Stream[Double].......

   */

  /*
   covariant type A occurs in contravariant position in type  <: A of type B
   */

  def startsWith[B >: A](sa2: fpinscala.laziness.Stream[B]): Boolean = {
    val sa1: fpinscala.laziness.Stream[A] = this
    /* perhaps references to Stream in this object have been intercepted
     by Collections' Stream:
     found: fpinscala.monads.Monad[scala.collection.immutable.Stream]  
     required: fpinscala.monads.Monad[fpinscala.laziness.Stream]

     
     */
    // val streamMonad: fpinscala.monads.Monad[fpinscala.laziness.Stream] =
    //   fpinscala.monads.Monad.streamMonad
    // val zipped: fpinscala.laziness.Stream[Tuple2[A,B]] =
    //   streamMonad.zip(sa1, sa2)

    val zipped: Stream[(A,B)] = sa1.zip(sa2)
    zipped.forAll{
      (tpl: Tuple2[A,B]) => tpl._1 == tpl._2
    }
  }

  /*
   'Zip' cannot be implemented with Monad's 'product.'
   
   But see Applicative's zip method.  Section 12.7.3 comments on
   differing "shapes" of functors to be zipped.
   */

  def zip[B](sb: fpinscala.laziness.Stream[B]):
      fpinscala.laziness.Stream[(A,B)] = {
    val sa: fpinscala.laziness.Stream[A] = this
    // Monad has no zip method
    // val streamMonad: Monad[fpinscala.laziness.Stream] =
    //   fpinscala.monads.Monad.streamMonad
    // val zipped: fpinscala.laziness.Stream[Tuple2[A,B]] =
    //   streamMonad.zip(sa, sb)
    // println("zipped")
    // zipped.feedback
    def zipHelper(
      streamA: fpinscala.laziness.Stream[A],
      streamB: fpinscala.laziness.Stream[B]
    ): fpinscala.laziness.Stream[(A,B)] =
      (streamA, streamB) match {
        case (
          fpinscala.laziness.Stream.cons(h1,t1),
          fpinscala.laziness.Stream.cons(h2,t2)
        ) => fpinscala.laziness.Stream.cons(
          (h1,h2), zipHelper(t1,t2)
        )
        case (
          _,
          fpinscala.laziness.Stream.cons(h2,t2)
        ) => fpinscala.laziness.Stream.empty
        case (
          fpinscala.laziness.Stream.cons(h1,t1),
          _
        ) => fpinscala.laziness.Stream.empty
        case (_,_) => fpinscala.laziness.Stream.empty
      }

    zipHelper(sa,sb)

  }


}
case object Empty extends fpinscala.laziness.Stream[Nothing]
case class Cons[+C](h: () => C, t: () => fpinscala.laziness.Stream[C]) extends fpinscala.laziness.Stream[C]

object Stream {
  // def cons[C](hd: => C, tl: => fpinscala.laziness.Stream[C]): fpinscala.laziness.Stream[C] = {
  //   lazy val head = hd
  //   lazy val tail = tl
  //   Cons(() => head, () => tail)
  // }
  object cons {
    def apply[C](hd: => C, tl: => fpinscala.laziness.Stream[C]): fpinscala.laziness.Stream[C] = {
    lazy val head = hd
    lazy val tail = tl
    fpinscala.laziness.Cons(() => head, () => tail)
  }

    def unapply[C](cs: fpinscala.laziness.Cons[C]):
        Option[Tuple2[C,fpinscala.laziness.Stream[C]]] =
      Some((cs.h(), cs.t()))
  }
  /*
   ^^^^^^^^^
   Section 5.2.1

   We typically want to cache the values of a Cons node, once they are forced. If we use the Cons data constructor directly, for instance, this code will actually compute expensive(x) twice:

   val x = Cons(() => expensive(x), tl) 
   val h1 = x.headOption 
   val h2 = x.headOption 

   We typically avoid this problem by defining smart constructors, which is what we call a function for constructing a data type that ensures some additional invariant or provides a slightly different signature than the “real” constructors used for pattern matching. By convention, smart constructors typically lowercase the first letter of the corresponding data constructor. Here, our cons smart constructor takes care of memoizing the by-name arguments for the head and tail of the Cons. This is a common trick, and it ensures that our thunk will only do its work once, when forced for the first time. Subsequent forces will return the cached lazy val: 
   
   def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {   
     lazy val head = hd   
     lazy val tail = tl   
     Cons(() => head, () => tail)
   } 

   The empty smart constructor just returns Empty, but annotates Empty as a Stream[A], which is better for type inference in some cases.

   We can see how both smart constructors are used in the Stream.apply function.  Recall that Scala uses subtyping to represent data constructors, but we almost always want to infer Stream as the type, not Cons or Empty. Making smart constructors that return the base type is a common trick. 

   def apply[A](as: A*): Stream[A] =   
     if (as.isEmpty) empty   
     else cons(as.head, apply(as.tail: _*)) 

   Again, Scala takes care of wrapping the arguments to cons in thunks, so the as.head and apply(as.tail: _*) expressions won’t be evaluated until we force the Stream.
 */
  def empty[C]: fpinscala.laziness.Stream[C] = Empty

  def apply[A](as: A*): fpinscala.laziness.Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: fpinscala.laziness.Stream[Int] = fpinscala.laziness.Stream.cons(1, ones)
  def constant[A](a: A): fpinscala.laziness.Stream[A] = fpinscala.laziness.Stream.cons(a, constant(a))
  def from(n: Int): fpinscala.laziness.Stream[Int] = fpinscala.laziness.Stream.cons(n, from(n+1))
  def fibs: fpinscala.laziness.Stream[Int] = fibs(0,1)
  def fibs(n0: Int, n1: Int): fpinscala.laziness.Stream[Int] = fpinscala.laziness.Stream.cons(n0+n1, fibs(n1, n0+n1))

  // def fibs: Stream[Int] = 
  //   Stream.cons(0,
  //     Stream.cons(1,
 

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): fpinscala.laziness.Stream[A] =
    f(z) match {
      case None => empty[A]
      case Some((a, s)) => fpinscala.laziness.Stream.cons(a, unfold(s)(f))
    }

  def _constant[A](a: A): fpinscala.laziness.Stream[A] = unfold(a)((a1: A) => Some(a1,a1))
  def _from(n: Int): fpinscala.laziness.Stream[Int] = unfold(n)((n0: Int) => Some(n0, n0+1))
  def _fibs: fpinscala.laziness.Stream[Int] = unfold((0,1))(
    (tpl: (Int,Int)) => Some(tpl._1 + tpl._2, (tpl._2, tpl._1 + tpl._2))
  )

  // ASCII characters 0x21 to 0x7E
  def characters: Stream[Char] = unfold(0x21){(i: Int)=>
    if(i<=0x7E) Some((i.toChar, i+1)) else None
  }

  def seq[A](start: => A, increment: A => A): Stream[A] =
    Cons(() => start, () => seq(increment(start), increment))

  def seq[A](start: => A, increment: A => A, end: A): Stream[A] =
    if(start!=end)
      Cons(() => start, () => seq(increment(start), increment))
    else
      Cons(() => start, () => Empty)


  // If `f` doesn't evaluate its second argument, the recursion never occurs
  // def until[A](st: Stream[A], end: A): Stream[A] =
  //   st.foldRight(Empty)((sa


}

object StreamTests {
  def main(args: Array[String]): Unit = {
    println("some Fibonacci numbers")
    /*
     Note that _fibs.toList(10) did not produce a compiler error,
     even though no function toList(Int) exists.
     I think this is because the type system assumes (10) is an application
     to a function that toList will become, at some point.
     How is this not known to be untrue at compile time?
     */
    println(Stream._fibs.toListFinite(10))

    // println("Fibonacci numbers mapped to Chars")
    // println(Stream._fibs.map((i: Int) => i.toChar).toListFinite(20))

    println("numbers to ASCII")
    println(Stream.from(1).map(_.toChar).toListFinite(70))

    val asciiNumbers = (65 to 91).toList
    println("test of unfold")
    val f: Int => Option[(Char, Int)] = 
      (i: Int) => if(i>=65 && i<=90) Some((i.toChar, i+1)) else None

    val letters: Stream[Char] = Stream.unfold(65)(f)
    println(letters)
    println("to list of length 20")
    println(letters.toListFinite(20))

    println("no letters")
    val noLetters: Stream[Char] = Stream.unfold(20)(f)
    println(noLetters)
    println("to list of length 20")
    println(noLetters.toListFinite(20))

    println("zipping")
    val zipped: Stream[(Int,Int)] = Stream.from(1).zip(Stream._fibs)
    println(zipped.toListFinite(20))

    val zippedletters: Stream[(Int,Char)] = Stream.from(1).zip(letters)

    println("zipped letters")
    println(zippedletters.toListFinite(20))

    println("starts with")
    println(letters.toListFinite(10))
    println("and")
    println(letters.toListFinite(10))
    println(letters.startsWith(letters))

    println("starts with")
    println(letters.toListFinite(10))
    println("and")
    println(Stream._fibs.toListFinite(10))
    println(letters.startsWith(Stream._fibs))

    println("find char M")
    println(letters.find((c: Char) => {c=='&'}))

    println("characters")
    Stream.characters.feedback

    println("testing map2")
    println("concatenate incrementing numbers and letters into short sentences")
    println("'numbered letters'")
    val numberedLetters: Stream[String] =
      Stream._from(1).map2(Stream.characters){(i: Int, c: Char)=>{
        s"$i $c"
      }
      }
    numberedLetters.feedback

    println("'lettered numbers'")
    val letteredNumbers: Stream[String] =
      Stream.characters.map2(Stream._from(1)){(c: Char, i: Int)=>{
        s"$i $c"
      }
      }
    letteredNumbers.feedback

    val reverseNumbers: Stream[Int] =
      Stream.seq(150, (i:Int)=>i-1, 1)
    println("reversed numbers")
    reverseNumbers.feedback

  }

}

