package jessitron.bison.ranker

import org.scalacheck._
import jessitron.bison._
import scalaz.stream.Process

object CapitalizationRankerSpec extends Properties("CapitalizationRanker") {

  // weakness: expressiveness - what is it about this input that makes the Bison agree?
  property("Example: all capitals") = {
    // weakness: painful construction
    val oneTweet = IncomingTweet(TweetDetail("I Am So Happy", "some-tweet-id", TwitterUser("jessitron")))

    val result = Process(oneTweet) |> CapitalizationRanker()

    // weakness: we're only testing one tweet, and a process is designed to handle many
    val output = result.toList.map{_.asInstanceOf[IncomingTweet]}

    // weakness of ScalaCheck: painful to get assertions to print well
    output.size == 1 &&
    output.head.opinions.size == 1 &&
    output.head.opinions.head == Opinion(1.0, Some("I Agree Completely"))
    // weakness: does it always have to be exactly that?
  }

  property("Example: no capitals") = {
    // weakness: duplication. Lots of it.
    val oneTweet = IncomingTweet(TweetDetail("i am not happy at all", "some-tweet-id", TwitterUser("jessitron")))

    val result = Process(oneTweet) |> CapitalizationRanker()

    val output = result.toList.map{_.asInstanceOf[IncomingTweet]}

    output.size == 1 &&
    output.head.opinions.size == 1 &&
    output.head.opinions.head == Opinion(-1.0, None)
    // weakness: we didn't specify WHY this output is different.
    // and that means the code might be using the wrong reason, that just happens to get these cases right.
  }

}
