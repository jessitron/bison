package jessitron.bison

/*
 * Goal: persist the state of the rankers, and read that
 * state back in on initialization.
 * Ideally, construct the rankers out of the state.
 *
 * Restrictions and difficulties:
 *  - the rankers can't themselves persist the state, because
 *  they're Process1s, which are not Process[Task,_]. So they
 *  don't get to have side effects, because those effects could
 *  only be sneaky. No Sneaky side-effects is a goal of FP and Bison.
 *
 * Components:
 *  - The rankers need to emit their state at some sort of signal.
 *  RollCall will do for now. This needs to be part of the ranker template.
 *  - At the end of the pipeline, or somewhere after the rankers anyway, that
 *  needs to be written to storage.
 *  - At startup, read this file. Somehow use it to instantiate rankers.
 *
 * Choices:
 *  - use json serialization. Might as well.
 *  - store things to a file. It's easy and human-readable.
 *
 * YAGNI:
 *  - rankers with a state type other than RankerState.
 *  - back up old file before overwriting
 */


import  org.scalacheck._
import jessitron.bison.ranker._
import scalaz.stream._

object DurableSpec extends Properties("Durable state") {

  /*
   * All I really care about is: I can construct a pipeline, change
   * the state of a ranker, shut it down, start it up, and obtain
   * a ranker in that same state again.
   */
  property("the whole thing") = {

    // I don't actually care, as long as the state changes somehow
    val testRanker = new Ranker {
      def consider(tweet: IncomingTweet, state: RankerState):
         Option[Opinion] = {
           return Some(Opinion(4.0))
      }
    }

    // should I pass in the writer/reader?

    //val rankerPipeline = constructPipeline(testRanker)

    // put some tweets in a Process
    // follow them with RollCall, then end
    // send them through the pipeline
    // get the last RollCall out the end, and verify
    // (if we sent any tweets) that the end state is
    // different from the beginning

    // val rankerPipeline = constructPipelineFromState()

    // Put one RollCall in.
    // Get the resulting RollCall out. Verify that the
    // state is the same as at the end of the last run.

    true
  }



}

/*
 * Errata. I would like to express that a ranker can respond to
 * IncomingTweet || IncomingFavorite. This is an explicit union
 * type, eh? which Scala will do in a future version, according to Nada.
 * I love the people I meet at conferences.
 */
