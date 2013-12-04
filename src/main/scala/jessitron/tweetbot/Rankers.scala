package jessitron.tweetbot.ranker

import scalaz.stream._
import scalaz.concurrent.Task
import scala.util.Random
import jessitron.tweetbot._

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

case class RankerState(tweetsSeen: Int = 0,
                       tweetsRanked: Int = 0,
                       pointsGivenOut: Double = 0.0,
                       recommendationsAccepted: Int = 0,
                       targetAveragePoints: Double = 0.5
                      ) {
  def rankedATweet(points: Score) = {
    copy(tweetsSeen = tweetsSeen + 1,
         tweetsRanked = tweetsRanked + 1,
         pointsGivenOut = pointsGivenOut + points)
  }
  def passedATweet() = {
    copy(tweetsSeen = tweetsSeen + 1)
  }
  def likelyAmountOfPoints: Score =
    if (tweetsRanked < 1) targetAveragePoints
    else targetAveragePoints * (tweetsSeen / tweetsRanked)
}


trait Ranker {
  def matches: IncomingTweet => Boolean
  def opinionate(initial: RankerState): Process1[Message, Message]
}

object iDoThisToo extends Ranker {
  val FirstPersonSentence = ".*I ([^.!?]*).*".r

  val pf:PartialFunction[(Message, RankerState), Opinion] = {
    case (TweetText(FirstPersonSentence(words)), state) =>
         Opinion(state.likelyAmountOfPoints, Some(s"I $words, too!"))
  }

  def matches = pf.isDefinedAt(_, RankerState())

  def opinionate(initialState: RankerState = RankerState()): Process1[Message, Message] = {
    import Process._
    def go(state: RankerState): Process1[Message, Message] = {
      await1 flatMap { m: Message =>
        val opinionO:Option[Opinion] = pf.lift((m, state))
        val o = (opinionO, m) match {
          case (Some(opinion), i: IncomingTweet) => i.addMyTwoCents(opinion)
          case (_, rc@RollCall(_)) => rc.andMe(Notification("iDoThisToo", state))
          case (_, m) => m
        }
        val s = (m,opinionO) match {
          case (i : IncomingTweet, Some(opinion)) =>
            state.rankedATweet(opinion.points)
          case (i: IncomingTweet, None) =>
            state.passedATweet()
          case _ => state
        }
        emit(o) fby go(s)
      }
    }
    go(initialState)
  }
}

