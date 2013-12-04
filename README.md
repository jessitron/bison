"The bison's temperament is often unpredictable. They usually appear
peaceful, unconcerned, even lazy, yet they may attack anything, often
without warning or apparent reason." - wikipedia

Perfect for a twitterbot

# Bison

A scalaz-stream implementation of twitterbot, written for a talk at
CodeMesh.io

## Notes for using it

To make your own twitterbot, you'll need to create a twitter
application. Once you've done that, and have the keys for it, do
    sbt test:run
and choose AuthorizationSetup. This will step you through authorizing
your twitter user on the app, and also create the keys.json file that
the application will look for.

## Notes for building it

I'm on 0.3-SNAPSHOT of scalaz-stream, which isn't released at this time
(2 Dec 20130) so you'll have to fork that
library and do "sbt publish-local" to put 0.3 in your local ivy cache.

## The demo code
Here's what I ran during the talk, in the repl:

    import scalaz.stream._
    import Process._
    import scalaz.concurrent.Task
    import scala.concurrent.duration._
    import jessitron.tweetbot._

    val triggerTimes: Process[Task, Duration] = Process.awakeEvery(1.second)
    val triggers: Process[Task, Message] = triggerTimes.map{ d => println(s"triggering at $d"); TimeToTweet }.take(10)

    val randomTweets: Process[Task, IncomingTweet] = Process.eval{ Task.delay{ ranker.RankerSpec.realisticTweet.sample.get }}.repeat.take(50)

    val slowTweets: Process[Task,Message] = Process.every(400.millis).tee(randomTweets)(tee.when)

    def chooserInput: Process[Task, Message] = slowTweets.wye(triggers)(wye.merge[Message])

    import ranker._
    val allRankers = Rankers.randomo |> iDoThisToo.opinionate() |>
    Rankers.shortIsBetter |> Rankers.retweetsAreRightOut

    def rankedChooserInput = (slowTweets |> allRankers).merge(triggers)

    val chooseAndRespond = Chooser.tweetPicker(5) |> Respond.responder

    def randomTweets = rankedChooserInput |> chooseAndRespond

    // the following requires real twitter auth codes in keys.json
    val realInput = SearchInput("#codemesh") |> SearchInput.jsonToTweets

    def realTweetChoices = realInput.take(50).pipe(allRankers).merge(triggers).pipe(chooseAndRespond)

    def goBisonGo = realTweetChoices through TwitterConnection.outputChannel()

