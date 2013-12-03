package jessitron.tweetbot

import scalaz.stream._

import org.scalacheck._
import Prop._

object RespondSpec extends Properties("Respond") {
  import Common._

  property("The best response comes out") = forAll(someRespondTos) {
    respondRequests =>
      val input = Process(respondRequests:_*).toSource
      val subject = Respond.responder
      val output = (input |> subject).runLog.run

      val respondedTweets = output collect {
        case TweetThis(_, Some(i)) => i
      }

      val responseTexts = output collect {
        case TweetThis(OutgoingTweet(text), Some(i)) => (i.from, text)
      }

      val expectedTweets = respondRequests map {_.tweet} filter (Respond.containsSuggestion)

      (expectedTweets forall { respondedTweets.contains(_)}) :| "All response requests with suggestions came out as tweets" &&
      (responseTexts forall { case (user, text) => text.contains(user)}) :| "All responses contain the username" &&
      (responseTexts forall { case (_, text) => text.size <= 140}) :| "All responses are short enough"
      // test whether the best response was chosen? (do we care?)
  }

  // test containsSuggestion

}
