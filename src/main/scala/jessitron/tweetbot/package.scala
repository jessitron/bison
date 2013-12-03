package jessitron

import spray.json._
import DefaultJsonProtocol._

package object tweetbot {
  type Score = Double
  type TweetContents = String
  type TweetId = String
  type SuggestedText = Option[String]

  sealed trait Message
  case object TimeToTweet extends Message

  case class IncomingTweet(tweet: TweetDetail,
                           opinions: Seq[Opinion] = Seq()) extends Message {
     def id = tweet.id_str
     def from = s"@$tweet.user.screen_name"
     def totalScore = opinions map {_.points} sum
     def addMyTwoCents(o: Opinion) = copy(opinions = o +: opinions)
     def withOpinion(points:Score, text:TweetContents) = addMyTwoCents(Opinion(points, Some(text)))
  }
  object IncomingTweet {
    implicit val deserializer: JsonFormat[IncomingTweet] =
      new JsonFormat[IncomingTweet] {
        def read(js: JsValue) = {
          val tweet = js.convertTo[TweetDetail]
          IncomingTweet(tweet)
        }
        def write(i: IncomingTweet): JsValue = ???
      }
  }

  case class TweetThis(tweet: OutgoingTweet,
                       inReplyTo: Option[IncomingTweet] = None) extends Message
  case class OutgoingTweet(text: TweetContents, replyTo: Option[TweetId] = None)

  case class RespondTo(tweet:IncomingTweet) extends Message
  case object AllDone extends Message

  case class Opinion(points: Score, suggestedText: SuggestedText) {
    def hasSuggestion: Boolean = suggestedText.nonEmpty
  }
  case class TwitterUser(screen_name: String)
  object TwitterUser {
     implicit val format: JsonFormat[TwitterUser] = jsonFormat1(apply)
  }
  case class TweetDetail(text: TweetContents, id_str: TweetId, user: TwitterUser)
  case object TweetDetail {
     implicit val format: JsonFormat[TweetDetail] = jsonFormat3(apply)
  }
  object TweetText {
    def unapply(td: IncomingTweet): Option[String] = Some(td.tweet.text)
  }

  case class RollCall(whosHere: Seq[Notification[Any]] = Seq()) extends Message {
    def andMe[A](notice: Notification[A]) = copy(whosHere = notice +: whosHere)
  }
  case class Notification[+A](from: String, contents: A) extends Message
}
