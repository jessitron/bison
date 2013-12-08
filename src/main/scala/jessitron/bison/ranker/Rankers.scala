package jessitron.bison.ranker

import jessitron.bison._
import scalaz.stream._
import scalaz.concurrent.Task
import scala.util.Random

object Rankers {

  val randomo: Process1[Message, Message] = simpleOpinion{_ => Some(Opinion(Random.nextDouble, Some("I agree!")))}

  private def simpleOpinion(f: IncomingTweet => Option[Opinion]) = process1.lift { m: Message => m match {
    case i: IncomingTweet => f(i) match {
      case Some(opinion) => i.addMyTwoCents(opinion)
      case _ => i
    }
    case m => m
  }}

  val retweetsAreRightOut: Process1[Message, Message] =
    Process.await1[Message] flatMap {
      case i: IncomingTweet if i.retweet => Process.halt
      case m => Process.emit(m)
    } repeat

  def shortIsBetter: Process1[Message,Message] = simpleOpinion {
    i: IncomingTweet =>
      val points = (70 - i.tweet.text.size) * (2.0 / 70)
      Some(Opinion(points, Some("Rusty and I agree")))
  }

}



trait Ranker {
  def consider(t: IncomingTweet, s: RankerState): Option[Opinion]

  private type StateChange = RankerState => RankerState
  private val nothing: StateChange = {r => r}
  private def recordRankedTweet(opinion: Opinion): StateChange =
    { state => state.rankedATweet(opinion.points) }
  private val recordOtherTweet: StateChange = {state => state.passedATweet }

  def opinionate(initialState: RankerState = RankerState()): Process1[Message, Message] = {
    import Process._
    def go(state: RankerState): Process1[Message, Message] = {
      await1 flatMap { m: Message =>
        val (o: Message, stateChange: StateChange) = m match {
          case rc@RollCall(_) => (rc.andMe(Notification("iDoThisToo", state)), nothing)
          case i: IncomingTweet =>
            val opinionO:Option[Opinion] = consider(i, state)
            val o = opinionO match {
              case Some(opinion) => (i.addMyTwoCents(opinion), recordRankedTweet(opinion))
              case None => (m, recordOtherTweet)
            }
          case m => (m, nothing)
        }
        emit(o) fby go(stateChange(state))
      }
    }
    go(initialState)
  }
}

object iDoThisToo extends Ranker {
  val FirstPersonSentence = ".*I ([^.!?]*).*".r

  val pf:PartialFunction[(Message, RankerState), Opinion] = {
    case (TweetText(FirstPersonSentence(words)), state) =>
         Opinion(state.likelyAmountOfPoints, Some(s"I $words, too!"))
  }

  def consider(t: IncomingTweet, s: RankerState) = pf.lift(t,s)

}

