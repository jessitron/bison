package jessitron.bison.ranker

import org.scalacheck._
import jessitron.bison._
import scalaz.stream.Process
import Prop._

object ExampleSpecA1 extends Properties("CapitalizationRanker 1") {
  import TalkExamples._

  property("Example: all capitals") = {
    val oneTweet = tweetThatSays("I Am So Happy")

    val output = processThroughRanker(Seq(oneTweet))

    output.head.opinions.head == Opinion(1.0, Some("I Agree Completely"))
  }

}

object ExampleSpecA2 extends Properties("CapitalizationRanker 2") {
  import TalkExamples._

  property("Example: no capitals") = {
    val oneTweet = tweetThatSays("i am not happy at all")

    val output = processThroughRanker(Seq(oneTweet))

    val myOpinion = output.head.opinions.head
    (myOpinion == Opinion(-1.0, None)) :| s"Unexpected opinion: $myOpinion"
  }

}

object ExampleSpec2 extends Properties("CapitalizationRanker 2") {

  import Common._
  import Prop._
  property("Ranks tweets with more capitalized words higher") =
    forAll { listOfTweets: List[IncomingTweet] =>
    // well, we could add a general property of generators here.
    // We could also generate exactly one tweet, and then change it to have more capitalized words.
    // Even, to continue capitalizing and un-capitalizing, to make a whole list. The top of which has a suggestion.
     true
  }
}

object TalkExamples {
  def processThroughRanker(input: Seq[IncomingTweet]): List[IncomingTweet] = {
    val result = Process(input:_*) |> CapitalizationRanker()
    result.toList.map{_.asInstanceOf[IncomingTweet]}
  }

  def tweetThatSays(text: String) = IncomingTweet(TweetDetail(text, "some-tweet-id", TwitterUser("jessitron")))


}
