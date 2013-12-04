 package jessitron.tweetbot

 import scalaz.stream._
 import scalaz._
 import scalaz.concurrent.Task
 import scala.concurrent.duration._

 object Con4mationBison {
   // solution for this in process in scalaz-stream; meanwhile, mutation
   // as suggested by Paul
   def intersectMerge[A](p1: Process[Task,A],
                         p2: Process[Task,A]): Process[Task,A] =
  Process.suspend {
    import Process._
    import java.util.concurrent.atomic.AtomicBoolean
    val alive = new AtomicBoolean(true)
    p1.takeWhile(_ => alive.get).onComplete(eval_(Task.delay(alive.set(false)))).
    merge(
    p2.takeWhile(_ => alive.get).onComplete(eval_(Task.delay(alive.set(false)))))
  }

   def agree[A](source: Process[Task, String],
             sink: Channel[Task, Message, A],
             maxTweets: Int,
             tweetFrequency: Duration = 10 seconds): Process[Task, A] = {

       val (myTweetsQ, myTweetsS) = async.queue[Message]
       val incomingTweets = source |>
                            SearchInput.jsonToTweets
       val slowIncomingTweets = Process.every(750 millis).tee(incomingTweets)(tee.when)

       val reportState = Process.awakeEvery(3 seconds) map { _ => RollCall()}

       val rankingInput = intersectMerge(
                            intersectMerge(slowIncomingTweets, myTweetsS),
                            reportState)
       val rankedTweets = rankingInput |>
                          ranker.Rankers.randomo |>
                          ranker.Rankers.retweetsAreRightOut |>
                          ranker.iDoThisToo.opinionate(2.0) |>
                          ranker.Rankers.shortIsBetter

       val triggers = (Process.awakeEvery(tweetFrequency) map
                      {_ => TimeToTweet} take maxTweets)


       import TrackOwnTweets._
       (enqueueTweets(myTweetsQ) (
       intersectMerge(rankedTweets, triggers) |>
           dropTweeteds |>
           Chooser.tweetPicker(25) |>
           Respond.responder
         )) through sink

   }

 }
