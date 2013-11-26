package jessitron.tweetbot

import scalaz.stream._
import async.mutable.Queue
import scalaz.concurrent.Task

object TrackOwnTweets {

  def enqueueTweets(q: Queue[Message])(p: Process[Task, Message]): Process[Task, Message] = {
    val predicate: Message => Boolean = {m => m.isInstanceOf[Tweeted]}
    (p flatMap { m: Message =>
      if(predicate(m))
        Process.tell(m) ++ Process.emitO(m)
      else Process.emitO(m)
    }).drainW(async.toSink(q)) onComplete(Process.suspend {q.close; Process.halt})
  }

}
