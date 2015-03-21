
// https://www.youtube.com/watch?v=Jg3Uv_YWJqI
object Stateless extends SocialService {
  private val mutableCache = ...

  def followerStats(username: String) = {
    /*
     check cache
     if response exists and isn't state, return it
     otherwise compute new response:  call web service

     mutable cache is bad...
     how can we do this with Functional techniques?

     easiest way:  return copy of mutated cache with desired value; like passing RNG

     
     */

  }


  // state is being manually wired through all calls
  // error prone

  // example: easy to pass wrong copy of cache
  def followerStats(username: String, c: Cache):
      (Cache, FollowerStats) = {
    val (c1, ofs) = checkCache(u, c)
    ofs match {
      case Some(fs) => (c1, fs) // same return type
      case None => retrieve(u, c1) // in both cases...
    }
  }

  def checkCache(u: String, c: Cache):
      (Cache, Option[FollowerStats]) = ...

  def retrieve(u: String, c: Cache):
      (Cache, FollowerStats) = ...

 

  trait State[S, +A] {
    def run(initial: S): (S, A)  // leave as abstract
    def map[B](f: A => B): State[S, B] = {
      // (s0: State) => {
      //   val (s1, a) = this.run(s0)
      //   (s1, f(a))
      // }

      // State.apply(
      //   (s0: State) => {
      //     val (s1, a) = this.run(s0)
      //     (s1, f(a))
      //   }
      // )
      // or
      State(
        (s0: State) => {
          val (s1, a) = this.run(s0)
          (s1, f(a))
        }
      )

      /*
       this does not invoke the only function in the class.
       this invokes the apply method.

       can the apply method be inferred?
       */

    }
    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      (s0: State) => {
        val (s1, a) = this.run(s0)
        // relation between s1 and s2 is not just through a
        // it is through the 'run' call
        // not done properly in FPInScala State flatMap
        val (s2, b) = f(a).run(s1)
        (s2, b)
      }
    }

  }

  object State {
    def apply[S, A](f: S => (S, A)): State[S, A] = 
      new State[S, A] {
        def run(i: S) = f(i)
      }
  }

  // refactor the Follower Stats problem with State
  // use partial application

  def followerStats(u: String)(c: Cache):
      (Cache, FollowerStats) = {
    State(checkCache(u)) flatMap { 
      (ofs: Option[FollowerStats]) => 
      ofs match {
        case Some(fs) =>
          State { s => (s, fs) }
        case None =>
          State(retrieve(u))
      }: State[Cache, FollowerStats]
    }.run(c): (Cache, FollowerStats)
  }

  def followerStatsState(u: String):
      State[Cache, FollowerStats] = {
    State(checkCache(u)) flatMap { 
      (ofs: Option[FollowerStats]) => 
      ofs match {
        case Some(fs) =>
          State { s => (s, fs) }
        case None =>
          State(retrieve(u))
      }: State[Cache, FollowerStats]
    }: State[Cache, FollowerStats]
  }



  def checkCache(u: String)(c: Cache):
       (Cache, Option[FollowerStats])

  def checkCacheState(u: String):
       State[Cache, Option[FollowerStats]] = State { c =>
    c.get(u) match {
      case Some(Timestamped(fs, ts))
          if !stale(ts) =>
        (c.copy(hits = c.hits + 1), Some(fs))
      case other =>
        (c.copy(misses = c.misses + 1), None)
    }
  }


  def retrieve(u: String)(c: Cache): 
      (Cache, FollowerStats)

  def retrieveState(u: String)(c: Cache): 
      State[Cache, FollowerStats] = State { c =>
    val fs: FollowerStats = callWebService(u)
    val tfs: Timestamped[FollowerStats] = Timestamped(fs, now)
    (c.update(u, tfs), fs)
  }

  def followerStats(u: String): FollowerStats = for {
    ofs: Option[FollowerStats] <- checkCacheState(u)
    fs: FollowerStats <- ofs match {
      case Some(fs) =>
        State { (state: State) => (state, fs) }:
            State[Cache, FollowerStats]
      case None =>
        retrieveState(u): State[Cache, FollowerStats]
    }
  } yield fs: FollowerStats




}
