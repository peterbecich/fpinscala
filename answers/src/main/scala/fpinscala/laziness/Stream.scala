package fpinscala.answers.laziness

import Stream._
trait Stream[+A] {

  // The natural recursive solution
  def toListRecursive: List[A] = this match {
    case Cons(h,t) => h() :: t().toListRecursive
    case _ => List()
  }

  /*
  The above solution will stack overflow for large streams, since it's
  not tail-recursive. Here is a tail-recursive implementation. At each
  step we cons onto the front of the `acc` list, which will result in the
  reverse of the stream. Then at the end we reverse the result to get the
  correct order again.
  */
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  /*
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead. Note that the mutable
  list buffer never escapes our `toList` method, so this function is
  still _pure_.
  */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  /*
    Create a new Stream[A] from taking the n first elements from this. We can achieve that by recursively
    calling take on the invoked tail of a cons cell. We make sure that the tail is not invoked unless
    we need to, by handling the special case where n == 1 separately. If n == 0, we can avoid looking
    at the stream at all.
  */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  /*
    Create a new Stream[A] from this, but ignore the n first elements. This can be achieved by recursively calling
    drop on the invoked tail of a cons cell. Note that the implementation is also tail recursive.
  */
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /*
  It's a common Scala style to write method calls without `.` notation, as in `t() takeWhile f`.
  */
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }
  // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h,t) =>
        println(h())
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }
  }
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  /*
  Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a nonmatching element is found.
  */
  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a,b) => f(a) && b)

  def takeWhile_1(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if (f(h)) cons(h,t)
      else      empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def map2[B,C](sb: Stream[B])(f: (A,B)=>C): Stream[C] = {
    val sab: Stream[(A,B)] = this.zip(sb)
    //val merge: Tuple2[A,B] => C = ((a,b))=>f(a,b)
    def merge(tpl: Tuple2[A,B]): C = f(tpl._1, tpl._2)
    val sc: Stream[C] = sab.map(merge)
    sc
  }
  

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if (f(h)) cons(h, t)
      else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h,t), 1) => Some((h(), (empty, 0)))
      case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zip`
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))


  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  /*
  `s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point that `s2` is exhausted. If `s` is exhausted first, or we find an element that doesn't match, we terminate early. Using non-strictness, we can compose these three separate logical steps--the zipping, the termination when the second stream is exhausted, and the termination if a nonmatching element is found or the first stream is exhausted.
  */
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  /*
  The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
  */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  /*
  The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.

  The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
   */

  // def scanRightF[B](f: (A, => B) => B)(a: A, p0: => (B, Stream[B])) = {
  //     // lazy val p1 = p0
  //     // val b2 = f(a, p1._1)
  //     // (b2, cons(b2, p1._2))

  //     val b2 = f(a, p0._1)
  //     (b2, cons(b2, p0._2))
    
  // }

  // def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
  //   foldRight((z, Stream(z)))(scanRightF(f))._2

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      println(p1)
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

  def feedback: Unit = 
    printer(30)
  def print(n: Int): Unit = {
    def f(a: A, remaining: => Int): Int = {
      println(a)
      remaining - 1
    }
    this.take(n).foldRight(n)(f)
  }

  def printer(n: Int): Unit =
    this match {
      case Stream.cons(head, lazyTail) if n > 0 => {
        println(head)
        lazyTail.printer(n-1)
      }
      case Stream.cons(head, lazyTail) if n <= 0 => println("printed out number of elements requested")
      case Empty => println("reached end of Stream")
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

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  // def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
  //   lazy val head = hd
  //   lazy val tail = tl
  //   Cons(() => head, () => tail)
  // }

  object cons {
    def apply[C](hd: => C, tl: => Stream[C]): Stream[C] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def unapply[C](cs: Cons[C]):
        Option[Tuple2[C,Stream[C]]] =
      Some((cs.h(), cs.t()))
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // This is more efficient than `cons(a, constant(a))` since it's just
  // one object referencing itself.
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  /*
  The below two implementations use `fold` and `map` functions in the Option class to implement unfold, thereby doing away with the need to manually pattern match as in the above solution.
   */
  def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A])((p: (A,S)) =>
      cons(p._1,unfoldViaFold(p._2)(f)))

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((p: (A,S)) =>
      cons(p._1,unfoldViaMap(p._2)(f))).getOrElse(empty[A])

  /*
  Scala provides shorter syntax when the first action of a function literal is to match on an expression.  The function passed to `unfold` in `fibsViaUnfold` is equivalent to `p => p match { case (f0,f1) => ... }`, but we avoid having to choose a name for `p`, only to pattern match on it.
  */
  val fibsViaUnfold =
    unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }

  def fromViaUnfold(n: Int) =
    unfold(n)(n => Some((n,n+1)))

  def constantViaUnfold[A](a: A) =
    unfold(a)(_ => Some((a,a)))


  def _constant[A](a: A) = constantViaUnfold(a)

  def _from(n: Int) = fromViaUnfold(n)

  def _fibs = fibsViaUnfold

  // could also of course be implemented as constant(1)
  val onesViaUnfold = unfold(1)(_ => Some((1,1)))

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

  
}


object StreamTests {
  // [error]  found   : (Int, Int) => Int
  // [error]  required: (Int, => Int) => Int
  // def sum(streamI: Stream[Int]): Int =
  //   streamI.foldRight(0){ (i: Int, s: Int) => i+s }

  def lazySum(i: Int, acc: => Int): Int = i+acc

  def sum2(streamI: Stream[Int]): Int =
    streamI.foldRight(0)(lazySum _)

  def main(args: Array[String]): Unit = {
    // println("some Fibonacci numbers")
    // /*
    //  Note that _fibs.toList(10) did not produce a compiler error,
    //  even though no function toList(Int) exists.
    //  I think this is because the type system assumes (10) is an application
    //  to a function that toList will become, at some point.
    //  How is this not known to be untrue at compile time?
    //  */
    // println(Stream._fibs.toListFinite(10))

    // // println("Fibonacci numbers mapped to Chars")
    // // println(Stream._fibs.map((i: Int) => i.toChar).toListFinite(20))

    // println("numbers to ASCII")
    // println(Stream.from(1).map(_.toChar).toListFinite(70))

    // val asciiNumbers = (65 to 91).toList
    // println("test of unfold")
    // val f: Int => Option[(Char, Int)] = 
    //   (i: Int) => if(i>=65 && i<=90) Some((i.toChar, i+1)) else None

    // val letters: Stream[Char] = Stream.unfold(65)(f)
    // println(letters)
    // println("to list of length 20")
    // println(letters.toListFinite(20))

    // println("no letters")
    // val noLetters: Stream[Char] = Stream.unfold(20)(f)
    // println(noLetters)
    // println("to list of length 20")
    // println(noLetters.toListFinite(20))

    // println("zipping")
    // val zipped: Stream[(Int,Int)] = Stream.from(1).zip(Stream._fibs)
    // println(zipped.toListFinite(20))

    // val zippedletters: Stream[(Int,Char)] = Stream.from(1).zip(letters)

    // println("zipped letters")
    // println(zippedletters.toListFinite(20))

    // println("starts with")
    // println(letters.toListFinite(10))
    // println("and")
    // println(letters.toListFinite(10))
    // println(letters.startsWith(letters))

    // println("starts with")
    // println(letters.toListFinite(10))
    // println("and")
    // println(Stream._fibs.toListFinite(10))
    // println(letters.startsWith(Stream._fibs))

    // println("find char M")
    // println(letters.find((c: Char) => {c=='&'}))

    // println("characters")
    // Stream.characters.feedback

    // println("testing map2")
    // println("concatenate incrementing numbers and letters into short sentences")
    // println("'numbered letters'")
    // val numberedLetters: Stream[String] =
    //   Stream._from(1).map2(Stream.characters){(i: Int, c: Char)=>{
    //     s"$i $c"
    //   }
    //   }
    // numberedLetters.feedback

    // println("'lettered numbers'")
    // val letteredNumbers: Stream[String] =
    //   Stream.characters.map2(Stream._from(1)){(c: Char, i: Int)=>{
    //     s"$i $c"
    //   }
    //   }
    // letteredNumbers.feedback

    // val reverseNumbers: Stream[Int] =
    //   Stream.seq(150, (i:Int)=>i-1, 1)
    // println("reversed numbers")
    // reverseNumbers.feedback

    // println("----------------------------------")
    // val s = sum2(Stream.from(0).take(10))

    // println("sum: "+s)

    // reverseNumbers.printer(16)

    // println("tails")

    // def g(s: Stream[Int], foo: => Unit) = s.printer(8)

    // Stream.from(0).tails.foldRight(())(g)


    println("----------------------------------")
    println("scanRight")
    Stream.from(0).take(6).scanRight(0)(_+_).printer(18)

    println("----------------------------------")
    println("scanRight average")

    import com.twitter.algebird.AveragedValue
    import com.twitter.algebird.AveragedGroup
    val incrementingNumbers: Stream[Int] = Stream.from(0)


    def h(i: Int, priorAverage: => AveragedValue) = {
      val out = AveragedGroup.plus(AveragedValue(1, i.toDouble), priorAverage)
      println(out.value)
      out
    }

    val average: Stream[AveragedValue] =
      incrementingNumbers.take(64).scanRight(AveragedGroup.zero)(h)

    println("average of incrementing numbers")

    average.print(128)


  }

}


