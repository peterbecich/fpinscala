package fpinscala.state

//type Rand[A] = State[RNG, A]

/*
case class Rand has case ancestor fpinscala.state.State, but case-to-case inheritance is prohibited. To overcome this limitation, use extractors to pattern match on non-leaf nodes.
case class Rand[A](run: RNG => (A, RNG)) extends State[RNG, A]
 */

/*
 Created by Rand type
 case class Rand[A](run: RNG => (A, RNG)) extends State
 */
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

  //override def toString: String =  // get state here

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  val printout: MachinePrintout = MachinePrintout(coins, candies)
}

case class MachinePrintout(coinsHeld: Int, candiesHeld: Int) {
  override def toString: String = s"Coins held: $coinsHeld  Candies held: $candiesHeld"
}

object State {
  val simple = RNG.Simple(321.toLong)

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


  /*
   Some notes from Runar
   https://forums.manning.com/posts/list/34792.page#p86615

   1. The `set` function returns a State action, which is itself a function. The type of `set(f(s))` is `State[S,Unit]`, which under the hood is a function of type `S => (Unit, S)`. The assignment to _ here means "ignore the Unit value".

   2. The for-comprehension is not a loop. It is syntactic sugar for calls to `map` and `flatMap`. So this code:

   for {
   s <- get
   _ <- set(f(s))
   } yield ()

   is equivalent to:

   get.flatMap(s => set(f(s)).map(_ => ()))

   That last call to `map` is actually completely redundant, so we can omit it:

   get.flatMap(s => set(f(s)))

   And in `State`, the implementation of `flatMap` is just a kind of function composition. So ultimately that for-comprehension can be simplified to this:

   State(s => ((), f(s))) 

   */

  type Rand[A] = State[RNG, A]

  def rand[A](f: Int => A): Rand[A] = {
    val transition = (rng: RNG) => {
      val intRNG: Tuple2[Int, RNG] = rng.nextInt
      val aRNG: Tuple2[A, RNG] = (f(intRNG._1), intRNG._2)
      aRNG
    }
    State(transition)
  }

  def nonNegativeInt = rand((i: Int) => i.abs)

  def double = rand((i: Int) => i.doubleValue/Int.MaxValue)




  // def randInt: Rand[Int] =
  // S = Machine
  // A = (Coins held, Candies held)
  // def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
  //   (machine: Machine) =>
  //   inputs.foldRight(
  //     //State[Machine, (Int,Int)](machine, (machine.coins,machine.candies))
  //     State.unit(machine)
  //   ){
  //     (nextInput: Input,
  //       (machine: Machine, (coinsHeld: Int, candiesHeld: Int))) => {
  //       machine.locked match {
  //         case false => {
  //           nextInput match {
  //             case Coin
  //           }
  //           case true => (machine, (coinsHeld, candiesHeld))
  //         }

  //           (machine0: Machine) =>
  //           inputs.foldRight(
  //             State.unit(machine0)
  //           ){
  //             (nextInput: Input,
  //               (machine: Machine, (coinsHeld: Int, candiesHeld: Int))) => {
  //               // flatMap: old Machine => new Machine and coins, candies tuple
  //             }
  //           }
  //       }
  //       //def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)]
  //     }
  //   }

  /*
   S = Machine
   A = MachinePrintout
   */



    def candyMachineTransition(currentMachine: Machine, input: Input): (MachinePrintout, Machine) =
      input match {
        case Coin if currentMachine.locked == true && currentMachine.candies > 0 =>
          val replacementMachine =
            Machine(locked = false, candies = currentMachine.candies, coins = currentMachine.coins + 1)
          val printout = replacementMachine.printout
          (printout, replacementMachine)
        case Coin =>
          val printout = currentMachine.printout
          (printout, currentMachine)

        case Turn if currentMachine.locked == false =>
          val replacementMachine =
            Machine(locked = true, candies = currentMachine.candies - 1, coins = currentMachine.coins)
          val printout = replacementMachine.printout
          (printout, replacementMachine)

        case Turn =>
          val printout = currentMachine.printout
          (printout, currentMachine)

      }

  def candyMachineState(input: Input): State[Machine, MachinePrintout] =
    State((machine: Machine) => candyMachineTransition(machine, input))
  

  // def simulateMachine(inputs: List[Input]): State[Machine, MachinePrintout] = {
  //   (machine: Machine) => {
  //     val initialMachineState: State[Machine, Unit] =
  //       set(machine)

  //     inputs.foldRight(initialMachineState){
  //       (nextInput: Input, machineState: State[Machine, MachinePrintout]) => {
  //         val currentCandies = get
  //         val currentLocked = currentMachine.locked
  //         nextInput match {
  //           case Coin if currentCandies > 0 && currentLocked == false =>


  def simulateMachine(inputs: List[Input]): State[Machine, List[MachinePrintout]] = {
    val listStates: List[State[Machine, MachinePrintout]] =
      inputs.map { (input: Input) => candyMachineState(input) }

    sequence(listStates)

  }

}


object StateExamples {// extends App {
  import State._

  val incrementer: State[Int, Unit] = modify {
    (i: Int) => i+1
  }



}


object SimpleCandyMachine extends App {
  import State._

  println("Candy machine with state transitions set up manually")

  val candyMachine = Machine(locked = true, candies = 10, coins = 0)

  println("Candy machine's initial state: "+candyMachine)

  val givenCoin: Tuple2[MachinePrintout, Machine] = candyMachineTransition(candyMachine, Coin)

  println("Candy machine after given coin: "+givenCoin._2)
  println("Machine printout: "+givenCoin._1)

  val givenTurn = candyMachineTransition(givenCoin._2, Turn)

  println("Candy machine after given turn: "+givenTurn._2)
  println("Machine printout: "+givenTurn._1)


}

object CandyMachine extends App {
  import State._

  val easyInputs: List[Input] = List(Coin, Turn, Coin, Turn, Coin, Turn)

  println(s"inputs: $easyInputs")

  val state1: State[Machine, List[MachinePrintout]] = simulateMachine(easyInputs)

  println(s"State from inputs: $state1")

  val initialMachine = Machine(locked = true, candies = 10, coins = 0)

  println(s"initial machine: $initialMachine")

  val out1: Tuple2[List[MachinePrintout], Machine] = state1.run(initialMachine)

  println(s"machine after inputs: ${out1._2}")
  println(s"machine printouts: ${out1._1}")

  println("----------------------------------------")


  val inputs2: List[Input] = List(Coin, Coin, Coin, Turn, Turn, Turn, Turn)

  println(s"inputs: $inputs2")

  val state2: State[Machine, List[MachinePrintout]] = simulateMachine(inputs2)

  println(s"State from inputs: $state2")

  // No need to redefine the inital machine -- it hasn't changed

  println(s"initial machine: $initialMachine")

  val out2: Tuple2[List[MachinePrintout], Machine] = state2.run(initialMachine)

  println(s"machine after inputs: ${out2._2}")
  println(s"machine printouts: ${out2._1}")


  println("----------------------------------------")


  val inputs3: List[Input] = List(Turn, Turn, Turn, Turn)

  println(s"inputs: $inputs3")

  val state3: State[Machine, List[MachinePrintout]] = simulateMachine(inputs3)

  println(s"State from inputs: $state3")

  // No need to redefine the inital machine -- it hasn't changed

  println(s"initial machine: $initialMachine")

  val out3: Tuple2[List[MachinePrintout], Machine] = state3.run(initialMachine)

  println(s"machine after inputs: ${out3._2}")
  println(s"machine printouts: ${out3._1}")



}
