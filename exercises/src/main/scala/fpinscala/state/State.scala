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

  val int: Rand[Int] = _.nextInt
  def randInt: Rand[Int] = (rng: RNG) => rng.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRNG) = rng.nextInt
    (nextInt.abs, nextRNG)
  }
                                             //RNG.map(rng)(_.abs)

  def double(rng: RNG): (Double, RNG) = {
    val (nextInt, nextRNG) = rng.nextInt
    val nextDouble = nextInt.doubleValue/Int.MaxValue
    (nextDouble, nextRNG)
  }
  def randDouble: Rand[Double] = (rng: RNG) => double(rng)

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


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {

    // difference between
    // (g: A) => Rand[B]
    // and
    // g: A => Rand[B]

    // Rand[A] = RNG => (A, RNG)
    // Rand[B] = RNG => (B, RNG)
    // g = A => (RNG => (A, RNG))

    (rng: RNG) => {
      val (valueA, rngA) = f(rng)
      val randB = g(valueA)
      randB(rngA)
    }
  }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
