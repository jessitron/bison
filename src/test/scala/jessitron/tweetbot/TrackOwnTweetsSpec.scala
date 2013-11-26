package jessitron.tweetbot

import scalaz.stream._

import org.scalacheck._
import Prop._

object TrackOwnTweetsSpec extends Properties("track what I am tweeting") {
  import Common._

  property("Tweeted gets added to the queue") = forAll(assortedMessages){
    messages: List[Message] =>
      val (queue, readProcess) = async.queue[Message]
      val subject = TrackOwnTweets.enqueueTweets(queue) _
      val output :Seq[Message]= subject(Process(messages:_*).toSource).runLog.run

      val queueContents = readProcess.runLog.run

      val tweeted = messages collect { case t: Tweeted => t}

      (output ?= messages.toSeq) :| "All messages should pass through" &&
      (tweeted forall { queueContents.contains(_)}) :| "all tweeted are queued" &&
      (tweeted.size =? queueContents.size) :| "nothing else is queued"
  }
}
