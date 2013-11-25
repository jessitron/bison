package jessitron

package object tweetbot {
  type Score = Double
  type TweetContents = String

  sealed trait Message
  case object TimeToTweet extends Message

  case class IncomingTweet(tweet: TweetDetail,
                           opinions: Seq[Opinion] = Seq()) extends Message {
     def totalScore = opinions map {_.points} sum
  }
  object IncomingTweet {
    import spray.json._
    import DefaultJsonProtocol._

    implicit val deserializer: JsonFormat[IncomingTweet] =
      new JsonFormat[IncomingTweet] {
        def read(js: JsValue) = {
          IncomingTweet(TweetDetail("Yoo hoo"))
        }
        def write(i: IncomingTweet): JsValue = {
          Map("text" -> i.tweet.text).toJson
        }
      }
  }

  case class TweetThis(tweet: TweetDetail,
                       inReplyTo: Option[IncomingTweet] = None) extends Message

  case class RespondTo(tweet:IncomingTweet) extends Message

  case class Opinion(points: Score)
  case class TweetDetail(text: TweetContents)
}
