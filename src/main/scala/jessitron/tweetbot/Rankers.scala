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

  val FirstPersonSentence = ".*I ([^!?]*)[\\.!?].*".r
  def iDoThisToo: Process1[Message, Message] = {
    import Process._
    def go(state: Double): Process1[Message, Message] = {
      await1 flatMap { m: Message =>
        val o = m match {
          case i@IncomingTweet(TweetDetail(FirstPersonSentence(words)),_) =>
            i.addMyTwoCents(Opinion(1.0, Some(s"I $words, too!")))
          case m => m
        }
        emit(o) fby go(state)
      }
    }
    go(1.0)
  }

}
