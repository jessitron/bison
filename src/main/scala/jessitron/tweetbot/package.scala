package jessitron

package object tweetbot {
  type Score = Double
  type TweetContents = String
  type SuggestedText = Option[String]

  sealed trait Message
  case object TimeToTweet extends Message

  case class IncomingTweet(tweet: TweetDetail,
                           opinions: Seq[Opinion] = Seq()) extends Message {
     def totalScore = opinions map {_.points} sum
     def addMyTwoCents(o: Opinion) = copy(opinions = o +: opinions)
  }
  object IncomingTweet {
    import spray.json._
    import DefaultJsonProtocol._

    implicit val deserializer: JsonFormat[IncomingTweet] =
      new JsonFormat[IncomingTweet] {
        def read(js: JsValue) = {
          val jso = js.asJsObject
          val text = jso.fields("text").toString
          IncomingTweet(TweetDetail(text))
        }
        def write(i: IncomingTweet): JsValue = ???
      }
  }

  case class TweetThis(tweet: TweetDetail,
                       inReplyTo: Option[IncomingTweet] = None) extends Message

  case class RespondTo(tweet:IncomingTweet) extends Message

  case class Opinion(points: Score, suggestedText: SuggestedText) {
    def hasSuggestion: Boolean = suggestedText.nonEmpty
  }
  case class TweetDetail(text: TweetContents)
}
