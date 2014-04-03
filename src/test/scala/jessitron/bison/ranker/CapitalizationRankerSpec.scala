package jessitron.bison.ranker

import org.scalacheck._
import jessitron.bison._
import scalaz.stream.Process

object CapitalizationRankerSpec extends Properties("CapitalizationRanker") {

  property("Example: all capitals") = {
    val oneTweet = IncomingTweet(TweetDetail("I Am So Happy", "some-tweet-id", TwitterUser("jessitron")))

    val result = Process(oneTweet) |> CapitalizationRanker()

    val output = result.toList.map{_.asInstanceOf[IncomingTweet]}

    output.size == 1 &&
    output.head.opinions.size == 1 &&
    output.head.opinions.head == Opinion(1.0, Some("I Agree Completely"))
  }

}
