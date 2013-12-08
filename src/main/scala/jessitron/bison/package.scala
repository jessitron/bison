package jessitron

import spray.json._
import DefaultJsonProtocol._

package object bison {
  type Score = Double
  type TweetContents = String
  type TweetId = String
  type SuggestedText = Option[String]

  sealed trait Message
  case object TimeToTweet extends Message

  case class IncomingTweet(tweet: TweetDetail,
                           opinions: Seq[Opinion] = Seq()) extends Message {
     def id = tweet.id_str
     def retweet: Boolean = tweet.retweeted_status.nonEmpty
     def from = s"@${tweet.user.screen_name}"
     def totalScore = opinions map {_.points} sum
     def addMyTwoCents(o: Opinion) = copy(opinions = o +: opinions)
     def withOpinion(points:Score, text:TweetContents) = addMyTwoCents(Opinion(points, Some(text)))

     override def toString: String = s"\nIncomingTweet: ${tweet.text}" +
     opinions.map{o => s"\n   ${o.points} points" +
        o.suggestedText.map(t => s", and you might say '$t'").getOrElse("")}.mkString("")
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
                       inReplyTo: Option[IncomingTweet] = None) extends Message {
    override def toString = (s"\nTweet This>>> ${tweet.text} <<< in response to" + inReplyTo)
  }
  case class OutgoingTweet(text: TweetContents, replyTo: Option[TweetId] = None)

  case class RespondTo(tweet:IncomingTweet) extends Message
  case object AllDone extends Message

  case class Opinion(points: Score, suggestedText: SuggestedText = None) {
    def hasSuggestion: Boolean = suggestedText.nonEmpty
  }
  case class TwitterUser(screen_name: String)
  object TwitterUser {
     implicit val format: JsonFormat[TwitterUser] = jsonFormat1(apply)
  }
  case class Retweeted(id_str:TweetId) // I would reuse TweetDetail except for a bug in spray-json. issue #77
  case object Retweeted {
     implicit val format: JsonFormat[Retweeted] = jsonFormat1(apply)
  }
  case class TweetDetail(text: TweetContents, id_str: TweetId, user: TwitterUser, retweeted_status: Option[Retweeted] = None)
  case object TweetDetail {
     implicit val format: JsonFormat[TweetDetail] = jsonFormat4(apply)
  }
  object TweetText {
    def unapply(td: IncomingTweet): Option[String] = Some(td.tweet.text)
  }

  case class RollCall(whosHere: Seq[Notification[Any]] = Seq()) extends Message {
    def andMe[A](notice: Notification[A]) = copy(whosHere = notice +: whosHere)
  }
  case class Notification[+A](from: String, contents: A) extends Message
}
