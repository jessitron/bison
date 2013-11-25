package jessitron.rdp

import org.scalacheck._
import scalaz.stream._
import scala.concurrent.duration._
/*
 * The end goal is a tweetbot for confirmation bison.
 *
 * tweets -> ranking -> ranking  -\
 *                                 -> choosing -> posting
 *                    triggering -/
 */

/*
 * Triggering: combine Process.every(10 seconds)
 * with whatever gives us the real value.
 * Here, explore scalaz-streams build-in stuff
 * to verify that it works like we want.
 *
 * And good thing I did; found two bugs and made two PRs.
 */
object TriggeringWye extends Properties("Triggering") {

  import process1.lift


  val triggeringGen: Gen[Process[Nothing, Unit]] = Gen.oneOf( Seq(Process( () )))

  val reasonableMillis: Gen[Duration] = Gen.choose(50, 300) map { _.millis }
  val delays: Gen[Seq[Duration]] = for {
    n <- Gen.choose(0, 6)
    list <- Gen.listOfN(n, reasonableMillis)
  } yield list

  import scala.concurrent.duration._
  import Process._
  def interval: Process1[Any, Duration] = {
    def go(t0: Long): Process1[Any, Duration] = {
      await1 flatMap {_:Any =>
        val now = System.nanoTime
        println("Now it is " + now + ". Before it was " + t0)
        emit((now - t0) nanos) ++ go(now)
      }
    }
    go(System.nanoTime)
  }

  /* This should really not run 100 times. It is slow */
  property("Regardless of when we pull from it, we never see a true twice within the same period") =
    Prop.forAll(reasonableMillis, delays)(stuff)

    def stuff: (Duration, Seq[Duration]) => Boolean = { (reasonableMillis, delays) =>
      // make a process that emits according to the scheduled delays
      val sleepyProcess = Process(delays:_*) flatMap { d => Process.sleep(d) ++ Process(d)}
      //
      // make the trigger. should spike true so often
      val trigger = Process.every(reasonableMillis)
      //
      // Combine the two.
      val combined = sleepyProcess.tee(trigger)(tee.zipWith { (l:Duration,r:Boolean) => r })
      //
      // Emit the durations between observed triggers
      // it might be better to do some sort of tee with durations, using only durations, and then
      // a process that executes a function on two values
      val durationsBetween = combined.filter(b => b) |> interval

      // those durations should never be less than the trigger interval
      println("1")
      println(combined.runLog.run)
      println("2")
      val tooLongDurations = durationsBetween.drop(1) filter { _ < reasonableMillis}
      println("3")
      val result = tooLongDurations.runLog.run.isEmpty
      println("4")
      result
    }

    property("this failed once") = Prop.exists{i: Int => stuff(99 millis, Seq(12 millis, 19 millis))}

}
