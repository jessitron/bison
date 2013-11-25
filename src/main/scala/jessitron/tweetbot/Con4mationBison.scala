 package jessitron.tweetbot

 import scalaz.stream._
 import scalaz._
 import scalaz.concurrent.Task
 import scala.concurrent.duration._

 object Con4mationBison {

   def agree(source: Process[Task, java.io.InputStream],
             sink: Sink[Task, Message],
             maxTweets: Int): Process[Task, Unit] = {

       val incomingTweets = (source flatMap (
                            SearchInput.streamToJson _)) |>
                            SearchInput.jsonToTweets
       val rankedTweets = incomingTweets |> Rankers.randomo
       val triggers = Process.awakeEvery(500 millis) map
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
