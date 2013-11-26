 package jessitron.tweetbot

 import scalaz.stream._
 import scalaz._
 import scalaz.concurrent.Task
 import scala.concurrent.duration._

 object Con4mationBison {

   def agree(source: Process[Task, String],
             sink: Sink[Task, Message],
             maxTweets: Int,
             tweetFrequency: Duration = 10 seconds): Process[Task, Unit] = {

       val incomingTweets = source |>
                            SearchInput.jsonToTweets
       val slowIncomingTweets = Process.every(1 second).tee(incomingTweets)(tee.when)
       val rankedTweets = slowIncomingTweets |> Rankers.randomo
       val triggers = Process.awakeEvery(tweetFrequency) map
                      {_ => TimeToTweet} take maxTweets

       val intersperse: Process1[Message \/ Message, Message] = process1.lift {
         m: Message \/ Message => m match {
         case -\/(m) => m
         case \/-(m) => m
       }}

       rankedTweets.wye(triggers)(wye.either) |> intersperse |>
           Chooser.tweetPicker(5) |>
           Respond.responder through sink

   }

 }
