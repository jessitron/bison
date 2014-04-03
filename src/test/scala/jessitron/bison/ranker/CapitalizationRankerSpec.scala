package jessitron.bison.ranker

import org.scalacheck._
import jessitron.bison._
import scalaz.stream.Process

object CapitalizationRankerSpec extends Properties("CapitalizationRanker") {

  // weakness: expressiveness - what is it about this input that makes the Bison agree?
  property("Example: all capitals") = {
    // weakness: painful construction
    val oneTweet = tweetThatSays("I Am So Happy")

    // weakness: we're only testing one tweet, and a process is designed to handle many
    val output: List[IncomingTweet] = processThroughRanker(Seq(oneTweet))

    // weakness of ScalaCheck: painful to get assertions to print well
    output.head.opinions.head == Opinion(1.0, Some("I Agree Completely"))
    // weakness: does it always have to be exactly that?
  }

  property("Example: no capitals") = {
    // weakness: duplication. Lots of it.
    val oneTweet = tweetThatSays("i am not happy at all")

    val output = processThroughRanker(Seq(oneTweet))

    output.size == 1 &&
    output.head.opinions.size == 1 &&
    output.head.opinions.head == Opinion(-1.0, None)
    // weakness: we didn't specify WHY this output is different.
    // and that means the code might be using the wrong reason, that just happens to get these cases right.
  }

  import Common._
  import Prop._

  property("Ranks tweets with more capitalized words higher") = forAll { listOfTweets: List[IncomingTweet] =>
    // well, we could add a general property of generators here.
    // We could also generate exactly one tweet, and then change it to have more capitalized words.
    // Even, to continue capitalizing and un-capitalizing, to make a whole list. The top of which has a suggestion.
     true
  }

  def processThroughRanker(input: Seq[IncomingTweet]): List[IncomingTweet] = {
    val result = Process(input:_*) |> CapitalizationRanker()
    result.toList.map{_.asInstanceOf[IncomingTweet]}
  }

  def tweetThatSays(text: String) = IncomingTweet(TweetDetail(text, "some-tweet-id", TwitterUser("jessitron")))


}
