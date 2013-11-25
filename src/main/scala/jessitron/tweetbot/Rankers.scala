package jessitron.tweetbot

import scalaz.stream._
import scala.util.Random

object Rankers {

  val randomo: Process1[Message, Message] = process1.lift { m: Message => m match {
    case i: IncomingTweet => i.addMyTwoCents(Opinion(Random.nextDouble, Some("I agree!")))
    case m => m
  }}

}
