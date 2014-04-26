package jessitron.bison

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
      val complainedTweets = output collect {
        case Notification(_, i : IncomingTweet) => i
      }

      val responseTexts = output collect {
        case TweetThis(OutgoingTweet(text, _), Some(i)) => (i.from, text)
      }

      val expectedTweets = respondRequests map {_.tweet}

      (expectedTweets forall { it: IncomingTweet =>
         respondedTweets.contains(it) || complainedTweets.contains(it)}) :|
      "All response requests with suggestions came out as tweets" &&
      (responseTexts forall { case (user, text) => text.contains(user)}) :| "All responses contain the username" &&
      (responseTexts forall { case (_, text) => text.size <= 140}) :| "All responses are short enough"
      // test whether the best response was chosen? (do we care?)
  }

  property("Simple example") = {
    val input = RespondTo(IncomingTweet(
      TweetDetail("He saw us!", "123", TwitterUser("lannisterc")),
      List(Opinion(1.0, Some("The things we do for love")))))

    val subject = Respond.responder
    val p = Process(input) |> subject
    val output = p.toList

    (output.length ?= 1) &&
    (output.head ?= TweetThis(
      OutgoingTweet("@lannisterc The things we do for love", Some("123")), Some(input.tweet)))
  }
  // test containsSuggestion

}
