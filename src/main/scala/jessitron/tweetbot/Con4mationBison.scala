 package jessitron.tweetbot

 import scalaz.stream._
 import scalaz._
 import scalaz.concurrent.Task
 import scala.concurrent.duration._

 object Con4mationBison {
   def intersperse(p1: Process[Task,Message],
                   p2: Process[Task, Message]): Process[Task, Message] =
    p1.wye(p2)(wye.either) |> process1.lift {
     m: Message \/ Message => m match {
     case -\/(m) => m
     case \/-(m) => m
    }}

   def haltIfAllDone: Process1[Message, Message] ={
     import Process._
     def go(): Process1[Message, Message] =
     await1 flatMap { m: Message =>
       m match {
         case AllDone => halt
         case m => emit(m) ++ go()
       }
     }
     go()
   }

   def agree(source: Process[Task, String],
             sink: Sink[Task, Message],
             maxTweets: Int,
             tweetFrequency: Duration = 10 seconds): Process[Task, Unit] = {

       val (myTweetsQ, myTweetsS) = async.queue[Message]
       val incomingTweets = source |>
                            SearchInput.jsonToTweets
       val slowIncomingTweets = Process.every(1 second).tee(incomingTweets)(tee.when)
       val rankingInput = intersperse(slowIncomingTweets,myTweetsS)
       val rankedTweets = rankingInput |> Rankers.randomo

       val triggers = (Process.awakeEvery(tweetFrequency) map
                      {_ => TimeToTweet} take maxTweets) ++ Process.emit(AllDone)


       import TrackOwnTweets._
       enqueueTweets(myTweetsQ) (
       intersperse(rankedTweets, triggers) |>
           haltIfAllDone |>
           dropTweeteds |>
           Chooser.tweetPicker(25) |>
           Respond.responder
         ) through sink

   }

 }
