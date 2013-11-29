package jessitron.tweetbot

import scalaz.stream._
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

  case class RankerState(tweetsSeen: Int = 0,
       tweetsRanked: Int = 0,
       pointsGivenOut: Double = 0.0,
       recommendationsAccepted: Int = 0,
       targetAveragePoints: Double = 1.0
      )

  val FirstPersonSentence = ".*I ([^!?]*)[\\.!?].*".r
  def iDoThisToo: Process1[Message, Message] = {
    import Process._
    def go(state: RankerState): Process1[Message, Message] = {
      await1 flatMap { m: Message =>
        val o = m match {
          case i@IncomingTweet(TweetDetail(FirstPersonSentence(words)),_) =>
            Seq(i.addMyTwoCents(Opinion(1.0, Some(s"I $words, too!"))))
          case EmitState => Seq(Notification("iDoThisToo", state), m)
          case m => Seq(m)
        }
        emitSeq(o) fby go(state)
      }
    }
    go(RankerState())
  }

  def shortIsBetter: Process1[Message,Message] = simpleOpinion {
    i: IncomingTweet =>
      val points = (70 - i.tweet.text.size) * (2.0 / 70)
      Some(Opinion(points, Some("Rusty and I agree")))
  }

}
