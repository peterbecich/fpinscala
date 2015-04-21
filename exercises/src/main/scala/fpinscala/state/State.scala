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

case class State[S,+A](run: S => (A, S)) {
  /* IS the case class syntactic sugar for: 

   https://twitter.github.io/scala_school/basics2.html#apply

   class State {
     def apply(run: S => (A,S)) = new State {
       def apply = run
     }
   }

   */
  def map[B](f: A => B): State[S, B] = {
    // State {
    //   (s0: S) => {
    //     val (a, s1) = this.run(s0)  // (S => (A,S))(s0) == (a, s1)
    //     (f(a), s1)
    //   }
    // }
    // implement with flatMap
    flatMap((a: A) => State((s: S) => (f(a),s)))

  }
  
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    // State {
    //   (s0: S) => {
    //     val (a, s1) = this.run(s0)
    //     val (b, s2) = sb.run(s1)
    //     (f(a,b), s2)
    //   }
    // }
    flatMap((a: A) => {
      // map: B => C
      sb.map((b: B) => f(a,b))
    })
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    // [error]  found   : S => fpinscala.state.State[S,B]
    // [error]  required: fpinscala.state.State[S,B]


    // uses an implicit `apply` method of State
    // case class State makes this `apply` method
    // automatically
    State {
      (s0: S) => {
        val (a, s1) = this.run(s0)
        f(a).run(s1)
      }
    }
  }



}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S,A](a: A): State[S,A] =
    State {
      (s: S) => (a, s)
    }

  def sequence[S,A](sas: List[State[S,A]]): State[S, List[A]] = {
    /*
     The main "leap" was knowing the type signature of this method.
     "List" is not abstracted away ... yet.
     */
    sas.foldRight {
      //State.unit(List[A]())
      // Where is this parameter to State[A], (S => (A, S)), specified?
      State((s: S)=>(List[A](),s))
    }{
      (state: State[S,A], stateList: State[S,List[A]])=>
      state.map2(stateList)((a: A, la: List[A]) => a::la)
    }
  }

  def modify[S](f: S => S): State[S, Unit] = {
    State.get.flatMap(s => State.set(f(s)))
  }


  /*
   At this point, there is some benefit to understanding how all Scala functions are objects:
https://twitter.github.io/scala_school/basics2.html#fnobj

Understanding this tells you how the apply method comes to be defined.
The number of arguments to apply is not dynamic -- it is fixed by the type of function that is inherited from.

These objects inherited from, Function0 through Function22, live in the namespace scala.*

Twitter's example:
object addPair extends Function2[Int, Int, Int] {
   def apply(m: Int, n: Int): Int = your definition
   }
equivalent to
def addPair(m: Int, n: Int): Int

To look only at the type signatures that our function addPair must conform to:
object foo extends Function2[A,B,C] {
  def apply(A, B): C
   }
equivalent to
def foo(A, B): C

As with parametric functions, you get to define the variable names,
so long as they conform to the parent's parametric types.


The same is true for Scala Classes
class AddOne extends Function[Int, Int] {
   def apply(m: Int): Int = m+1
   }

   */


  def get[S]: State[S,S] = State((s:S)=>(s,s))
  /*

   def get[S] = new State {
     def apply: 
     (scala.Function1[S, Tuple2[A, S]]) => fpinscala.state.State[S, A] 
     = ((s:S)=>(s,s)): scala.Function1[S, Tuple2[A, S]]

   */
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  /*

   def set[S](s: S): State[S, Unit] = new State {
     def apply:
     (scala.Function1[S, Tuple2[A, S]]) => fpinscala.state.State[S, A]
     = (() => ((), s)): (scala.Function1[S, Tuple2[A, S]])

   */

  /*
   Example in section 6.6 shows method `modify` as 
   For Comprehension

   def modify[S](f: S => S): State[S, Unit] = for {
     s <- get
     _ <- set(f(s))    // this 'unit' is very confusing to me
   // seeing the explicit expansion of this for comp.
    // to maps and flatmaps, below, should clarify it
   } yield ()  // unit

   equivalent to 
   
  def modify[S](f: S => S): State[S, Unit] = {
    State.get.flatMap(s => State.set(f(s)))
  }

   State[S, Unit] == 

   Note that the For Comprehension yields a unit,
   while the method returns a State[S, Unit].
   Somehow, the State object is returned implicitly!
   

   State.get takes no arguments and returns
   foo =  State {
       def run: (S => (A, S)) = 
         (s0: S) => (s0,s0)
     }: State[S,S]

   foo.flatMap(s1 => State.set(f(s1))) results in

   State {
       def run: (S => (A, S))
   = (s1: S) => State.set(f(s1))
   = (s1: S) => State.set(s2)
   = (s1: S) => State (
       def run = (dontCare: S) => (Unit, s2)
                      )
   
   
     

   */

  type Rand[A] = State[RNG, A]

//  def randInt: Rand[Int] = 
  // S = Machine
  // A = (Coins held, Candies held)
  // def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
  // (machine: Machine) =>
  // inputs.foldRight(
  //   //State[Machine, (Int,Int)](machine, (machine.coins,machine.candies))
  //   State.unit(machine)
  // ){
  //   (nextInput: Input,
  //     (machine: Machine, (coinsHeld: Int, candiesHeld: Int))) => {
  //     machine.locked match {
  //       case false => {
  //         nextInput match {
  //           case Coin
  //         }
  //         case true => (machine, (coinsHeld, candiesHeld))
  //       }

  //   (machine0: Machine) =>
  //   inputs.foldRight(
  //     State.unit(machine0)
  //   ){
  //     (nextInput: Input,
  //       (machine: Machine, (coinsHeld: Int, candiesHeld: Int))) => {
  //       // flatMap: old Machine => new Machine and coins, candies tuple
  //     }
  //   }
  // }
  //def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)]

}
