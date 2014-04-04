package jessitron.bison.ranker

import org.scalacheck._
import jessitron.bison._
import scalaz.stream.Process
import Prop._

object ExampleSpecA1 extends Properties("CapitalizationRanker A1") {
  import TalkExamples._

  property("Example: all capitals") = {
    val oneTweet = tweetThatSays("I Am So Happy")

    val output = processThroughRanker(Seq(oneTweet))

    output.head.opinions.head == Opinion(1.0, Some("I Agree Completely"))
  }

}

object ExampleSpecA2 extends Properties("CapitalizationRanker A2") {
  import TalkExamples._

  property("Example: no capitals") = {
    val oneTweet = tweetThatSays("i am not happy at all")

    val output = processThroughRanker(Seq(oneTweet))

    val myOpinion = output.head.opinions.head
    (myOpinion == Opinion(-1.0, None)) :| s"Unexpected opinion: $myOpinion"
  }

}

object ExampleSpecB2 extends Properties("CapitalizationRanker B2") {
  import Common._
  import TalkExamples._

  property("Adds exactly one opinion") =
    forAll { listOfTweets: List[IncomingTweet] =>
      val output = processThroughRanker(listOfTweets)
      val inputAndOutput = listOfTweets.zip(output)

      (listOfTweets.size == output.size) :| s"${listOfTweets.size} went in, ${output.size} came out!" &&
      (inputAndOutput.forall{
        case(in, out) => in.opinions.size == out.opinions.size - 1
      }) :| "Wrong number of opinions" &&
      inputAndOutput.forall{
        case(in, out) => in.tweet == out.tweet
      } :| "A tweet was altered!!" &&
      Prop.all(inputAndOutput.map{
        case(in, out) => Prop(in.opinions.forall(out.opinions.contains(_))) :|
                         s"An opinion was altered in tweet: $in"
      }:_*)
    }
}

object ExampleSpecX extends Properties("CapitalizationRanker 3") {
  import TalkExamples._
  import Common._
  import Prop._
  property("Ranks tweets with more capitalized words higher") =
    forAll(tweetDetail) { tweetDetail =>
      !firstWordIsCapitalized(tweetDetail) ==> {
        val moreCapital = capitalizeFirstWord(tweetDetail)

        val output = processThroughRanker(Seq(tweetDetail, moreCapital).map(IncomingTweet(_)))

        val Seq(lowerTweet, higherTweet) = output
        (lowerTweet.totalScore < higherTweet.totalScore) :|
           s"Capitalized tweet didn't get a higher score: ${lowerTweet.totalScore} ${higherTweet.totalScore}"
      }
  }

  def firstWordIsCapitalized(t: TweetDetail) = {
    // lame!
    t.text.charAt(0) >= 'A' && t.text.charAt(0) <= 'Z'
  }

  def capitalizeFirstWord(t: TweetDetail) = {
    // also lame!!
    val oldText = t.text
    val newText = oldText.substring(0,1).toUpperCase + oldText.substring(1)

    t.copy(text = newText)
  }
}

object TalkExamples {
  def processThroughRanker(input: Seq[IncomingTweet]): List[IncomingTweet] = {
    val result = Process(input:_*) |> CapitalizationRanker()
    result.toList.map{_.asInstanceOf[IncomingTweet]}
  }

  def tweetThatSays(text: String) = IncomingTweet(TweetDetail(text, "some-tweet-id", TwitterUser("jessitron")))


}
