package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
        (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  // uses type inference to guess argument to nextInt -- an RNG
  val int: Rand[Int] = _.nextInt

  

  // explicit
  def randInt: Rand[Int] = (rng: RNG) => rng.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  { rng0 =>
    val (a, rng1) = s(rng0)
    val b = f(a)
    (b, rng1)
  }

  def chooseInt(rng: RNG)(start: Int, stopExclusive: Int): (Int, RNG) = {
    //println(start + " to " + stopExclusive)
    val (nextNonNegativeInt, nextRNG) = RNG.nonNegativeInt(rng)
    val scale: Double = (nextNonNegativeInt.toDouble/Int.MaxValue)
    //println("scale: "+scale)
    //val nextChosenInt: Int = (start + (stopExclusive-start)*scale).toInt
    val nextChosenInt: Int = 
      (start + stopExclusive*scale - start*scale).toInt

    // probably a casting necessary somewhere in here
    // val nextChosenInt: Int = 
    //   (stopExclusive*scale - (start-1)*scale).toInt
    //println(nextChosenInt)
    // is this exclusive on the upper bound?
    (nextChosenInt, nextRNG)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }


  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRNG) = rng.nextInt
    (nextInt.abs, nextRNG)
  }

  /*
   Generates a double in [-1, 1]
   */
  def double(rng: RNG): (Double, RNG) = {
    val (nextInt, nextRNG) = rng.nextInt
    val nextDouble = nextInt.doubleValue/Int.MaxValue
    (nextDouble, nextRNG)
  }
  def randDouble: Rand[Double] = (rng: RNG) => double(rng)

  def nonNegativeDouble(rng: RNG): (Double, RNG) = {
    val (nextNonNegativeInt, rng1): Tuple2[Int, RNG] = RNG.nonNegativeInt(rng)
    val d: Double = (nextNonNegativeInt.toDouble/Int.MaxValue)
    (d, rng1)
  }

  def chooseDouble(rng0: RNG)(start: Double, stopExclusive: Double): (Double, RNG) = {
    val (nnDouble, rng1) = nonNegativeDouble(rng0)
    val diff = stopExclusive-start
    val scale: Double = diff
    val nextChosenDouble: Double = (scale*nnDouble)+start
    (nextChosenDouble, rng1)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    // val (nextInt, secondRNG) = rng.nextInt
   // val (nextDouble, thirdRNG) = RNG.double(secondRNG)

    // ((nextInt, nextDouble), thirdRNG)
    val intDoubleRand: Rand[(Int, Double)] =
      RNG.map2(
        (rngA: RNG) => rngA.nextInt,
        (rngB: RNG) => RNG.double(rngB)
      )((int: Int, dbl: Double) => (int, dbl))

    intDoubleRand(rng)
  }

  def doubleDouble(rng0: RNG): ((Double, Double), RNG) = {
    val (double1, rng1) = RNG.double(rng0)
    val (double2, rng2) = RNG.double(rng1)
    ((double1, double2), rng2)
  }

  def chooseDoubleDouble(rng0: RNG)(start: Double, stopExclusive: Double):
      ((Double, Double), RNG) = {
    val (double1, rng1) = chooseDouble(rng0)(start, stopExclusive)
    val (double2, rng2) = chooseDouble(rng1)(start, stopExclusive)
    ((double1, double2), rng2)
  }

  def radius(x: Double, y: Double): Double =
    math.sqrt(math.pow(x,2)+math.pow(y,2))

  def inCircle(circleRadius: Double)(rng0: RNG): (Boolean, RNG) = {
    val ((x,y), rng1) = chooseDoubleDouble(rng0)(-1,1)
    val pointRadius: Double = radius(x,y)
    if(pointRadius <= circleRadius) (true, rng1)
    else (false, rng1)
  }

  def approxPi(n: Int): Rand[Double] = { (rng0: RNG) =>
    val initialCount: Tuple2[Int, RNG] = (0, rng0)
    val inCircleCount: Tuple2[Int, RNG] =
      (0 to n).foldLeft(initialCount){(tup: Tuple2[Int, RNG], _: Int) =>
        val acc = tup._1
        val rng = tup._2
        val (inC, nextRNG) = inCircle(1.0)(rng)
        if(inC) (acc+1, nextRNG)
        else (acc, nextRNG)
      }
    val ratioInCircle: Tuple2[Double, RNG] = (inCircleCount._1.toDouble/n, inCircleCount._2)

    val pi = (ratioInCircle._1*4, ratioInCircle._2)

    pi: Tuple2[Double, RNG]
  }





  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (nextDouble, secondRNG) = RNG.double(rng)
    val (nextInt, thirdRNG) = secondRNG.nextInt
    ((nextDouble, nextInt), thirdRNG)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    /*
     It would be nice to figure out this solution with flatMap,
     fold, etc.
     */
    // for(i<-1:3;
    //   val (nextDouble, nextRNG){

    // }
    val ll = List.fill(3)(randDouble)

    ???
    
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val ll = List.fill(count)(randInt)

    // ll.foldRight(z: B)(op: Function2[Function1[Int], B, B])

    ll.foldRight((List[Int](), rng))(
      (nextRand: Rand[Int], l2: (List[Int], RNG)) => {
        val (nextInt, nextRNG) = nextRand(l2._2)
        (nextInt :: l2._1, nextRNG)
      }
    )
  }
  def list[A](count: Int)(rng: RNG)(rand: Rand[A]): (List[A], RNG) = {
    val ll = List.fill(count)(rand)
    ll.foldRight((List[A](), rng))(

      // [error] Note: Tuples cannot be directly destructured in method or function parameters.
      // [error]       Either create a single parameter accepting the Tuple1,
      // [error]       or consider a pattern matching anonymous function: `{ case (param1, param1) => ... }
      // [error]       (nextRand: Rand[A], (prevAList, prevRNG): (List[A], RNG)) => {
      // [error]                                               ^


      //       (nextRand: Rand[A], (prevAList, prevRNG): (List[A], RNG)) => {
      //         val (nextA, nextRNG) = nextRand(prevRNG)
      //         (nextA :: prevAList, nextRNG)
      //       }

      (nextRand: Rand[A], l2: (List[A], RNG)) => {
        val (nextA, nextRNG) = nextRand(l2._2)
        (nextA :: l2._1, nextRNG)
      }

    )
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  {
    // Return function RNG => (C, RNG)
    (rng: RNG) => {
      val (raValue, raRNG) = ra(rng)
      val (rbValue, rbRNG) = rb(raRNG)
      (f(raValue, rbValue), rbRNG)
    }

  }


  // def generate[A](f: Int => A): Rand[A] = { rng0 =>
  //   val (int1, rng1) = rng0.nextInt
  //   val a1 = f(int1)
  //   (a1, rng1)
  // }

  def generate[A](f: Int => A):
      Rand[A] =
    map(int)(f)

  /*
   Generates a double in [Int.MIN_VALUE, Int.MAX_VALUE]
   */
  val generateDouble = generate((i: Int) => i.toDouble)

  //@annotation.tailrec
  // def generateTrue[A](f: Int => A)(p: A => Boolean): Rand[A] = { rng0 =>
  //   val (a1, rng1) = generate(f)(rng0)
  //   if(p(a1))
  //     (a1, rng1)
  //   else
  //     generateTrue(f)(p)(rng1)
  // }

  def generateTrue[A](f: Int => A)(p: A => Boolean): Rand[A] = 
    flatMap(generate(f)){ a =>
      if(p(a)) unit(a)
      else generateTrue(f)(p)
    }

  // the best you can do, with only map
    // map(generate(f)){ a =>
    //   if(p(a)) unit(a): Rand[A]
    //   else generateTrue(f)(p): Rand[A]
    // }: Rand[Rand[A]]

  def rejectionSampler[A](ra: Rand[A])(f: A => Boolean): Rand[A] = ???

  
  /*
   Generates a double in [0, Int.MAX_VALUE]
   */
  val generatePositiveDouble = generateTrue((i: Int) => i.toDouble)((d: Double) => d>=0.0)

  /*
   Generates a double in [0, 1000000)
   */
  val generateSmallDouble = generateTrue((i: Int) => i.toDouble)((d: Double) => d.abs<1000000)


  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    RNG.map2(ra,rb){
      (a: A, b: B) => (a,b)
    }

  def randIntDouble: Rand[(Int, Double)] =
    RNG.both(RNG.randInt, RNG.randDouble)

  def randIntDoubleEquivalent: Rand[(Int, Double)] =
    RNG.both(RNG.randInt, RNG.double)

  def randIntDoubleEquivalent2: Rand[(Int, Double)] =
    RNG.both(RNG.randInt, RNG.double(_))


  def randDoubleInt: Rand[(Double, Int)] =
    RNG.both(RNG.randDouble, RNG.randInt)


  /*
   List[Rand[A]]
   Cons(Rand[A], Cons(Rand[A], Cons(Rand[A], Nil)))
   Cons((rng1)=>(a1,rng2), Cons(rng2=>(a2,rng3), Cons(rng3=>(a3,rng4), Nil)))

   
   Rand[List[A]]
   (rng1) => (Cons(a1,Cons(a2,Cons(a3,Nil))), rng4)

   sequence
   map2(


   Remember List has all the built-in methods that Rand lacks:
   fold
   flatMap
   etc.
   */

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    // fs.foldRight(z: B)(op: Function2[Function1[A], B, B])
    // fs.fold(z: A1)(op: Function2[A1, A1, A1])
    // fs.foldLeft(z: B)(f: Function2[B, Function1[A], B])
    
    fs.foldRight{
      RNG.unit(scala.collection.immutable.Nil): Rand[List[A]]
    }{
      (ra: Rand[A], rl: Rand[List[A]]) => {
        RNG.map2(ra, rl){(a: A, la: List[A]) =>
          a :: la
        }
      }
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {

    /*
    Is there a difference between
    (g: A) => Rand[B]
    and
    f: A => Rand[B]
    ?
     I think so, when it comes to passing.
     f would take the form
        def f(a: A): Rand[B]
     Can f


    Rand[A] = RNG => (A, RNG)
    Rand[B] = RNG => (B, RNG)
    g = A => (RNG => (A, RNG))
    g = (a: A) => ((rng: RNG) => (a, rng))
     */
    (rng: RNG) => {
      val (valueA, rngA) = f(rng)
      val randB = g(valueA)
      randB(rngA)
    }
  }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    //def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B]

    // this is 'unit'
    //val g: (A => Rand[B]) = (a: A) => ((rng: RNG) => (f(a), rng))
    val g: (A => Rand[B]) = (a: A) => RNG.unit(f(a))
    flatMap(s)(g): Rand[B]

  }
}

object RandomExamples extends App {
  import RNG._

  val simple = Simple(123.toLong)

  val lessThan10: Rand[Int] = nonNegativeLessThan(10)

  val ll = list(16)(simple)(lessThan10)

  println(ll)

  println("non-negative integer")

  val nni = list(16)(simple)(nonNegativeInt)

  println(nni)

  println("non negative double")
  val nnd = list(16)(simple)(nonNegativeDouble)
  println(nnd)


  println("choose double")

  val ll2 = list(16)(simple)((rng) => chooseDouble(rng)(-1,1))

  println(ll2)


  println("choose double double")

  val ll4 = list(16)(simple)((rng) => chooseDoubleDouble(rng)(-1,1))
  println(ll4)


  println("double double")
  val ll3 = list(16)(simple)(doubleDouble)

  println(ll3)

  val uniformInts = ints(1000)(simple)

  // import java.nio.file.{Paths, Files}
  // import java.nio.charset.StandardCharsets
  // import java.nio.file.StandardOpenOption
  //Files.write(Paths.get("uniform_ints.txt"), "foo".getBytes("utf-8"))

  import java.io.PrintWriter

  val pw = new PrintWriter("uniform_ints.txt")

  uniformInts._1.foldLeft(()){(_: Unit, i: Int) =>
    pw.write((i.toString()+"\n").toCharArray())
  }
  pw.close()

  println("approximation of pi")

  println("30 iterations")

  val pi30 = approxPi(30)(simple)

  println(pi30)
  
  println("300 iterations")

  val pi300 = approxPi(300)(simple)

  println(pi300)

  println("3000 iterations")

  val pi3000 = approxPi(3000)(simple)

  println(pi3000)

  println("300000 iterations")

  val pi300000 = approxPi(300000)(simple)

  println(pi300000)

  println("30000000 iterations")

  val pi30000000 = approxPi(30000000)(simple)

  println(pi30000000)



  println("generate positive double")

  val pd = list(32)(simple)(generatePositiveDouble)

  println(pd)

  println("generate double of ABS < 1000000")

  val d2 = generateSmallDouble(simple)

  println(d2)

  // val d3 = list(8)(simple)(generateSmallDouble)
  // println(d3)

}


