package jessitron.tweetbot

import scalaz.stream._
import spray.json._
import DefaultJsonProtocol._

object SearchInput {

  case class SearchResults(statuses: Seq[IncomingTweet])
  object SearchResults {
    implicit val jsonFormat: JsonFormat[SearchResults] = jsonFormat1(apply)
  }

  val jsonToTweets: Process1[String, IncomingTweet] =
    Process.await1 flatMap { string: String =>
      val parsed = string.asJson.convertTo[SearchResults]
      Process.emitSeq(parsed.statuses)
    }

}
