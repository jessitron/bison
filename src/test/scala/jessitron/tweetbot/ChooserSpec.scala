package jessitron.tweetbot

import scalaz.stream._

import org.scalacheck._
import Gen._
import Prop._

object ChooserSpec extends Properties("Chooser") {

  import Common._

  property("Gives back saved tweets") = forAll(smallInt, someIncomingTweets, smallInt) {
    (poolSize: Int, incomingTweets: List[IncomingTweet], triggerCount: Int) =>

      val triggers = Process.fill(triggerCount)(TimeToTweet)
      val tweets = Process(incomingTweets:_*).toSource
      val source = tweets ++ triggers

      val subject = Chooser.tweetPicker(poolSize)
      val output = (source |> subject).runLog.run
      val results = output.toList groupBy { case i: RespondTo => "tweet"
                                            case t@ TimeToTweet => "trigger"
                                            case _ => "other" }

      val expectedTweetQty = poolSize min incomingTweets.size min triggerCount
      val expectedTweetOrder = incomingTweets sortBy {_.totalScore * -1 } take expectedTweetQty
      val expectedTriggers = (triggerCount - expectedTweetQty) max 0

      val receivedTweets = results.getOrElse("tweet", Seq()).map{_.asInstanceOf[RespondTo].tweet}
      val receivedTriggers = results.getOrElse("trigger", Seq())

      (receivedTweets.size ?= expectedTweetQty)   :| "as many tweets as received, stored, and triggered are output" &&
      (receivedTweets ?= expectedTweetOrder)      :| "tweets come out highest score first" &&
      (receivedTriggers.size ?= expectedTriggers) :| "extra triggers pass through" &&
      (results.get("other") ?= None)              :| "nothing else comes out"
        // perhaps we should pass through incoming tweets as well?
  }

}
