package jessitron.bison.ranker

import jessitron.bison._
import scalaz.stream._

import org.scalacheck._
import Prop._

object RankerSpec extends Properties("Rankers") {
  import Common._

  property("The random ranker always adds an opinion") = forAll(incomingTweet){
    incoming: IncomingTweet =>
      val p = Process(incoming) |> Rankers.randomo
      val output = p.toList collect { case i: IncomingTweet => i}

      (output.size ?= 1) :| "Something always comes out" &&
      (output.head.opinions.size ?= (1 + incoming.opinions.size)) :| "One more opinon"
  }

  import Gen._
  val presentTenseSentence: Gen[String] = for {
    subj <- oneOf("I", "you", "@jessitron", "We", "My mom")
    verb <- oneOf("love", "eat", "run", "code")
    adverb <- oneOf("just","well","swimmingly","crazily")
    preposition <- oneOf("with","in","among")
    noun <- oneOf("pizza","pillows","babies","scala")
    adjective <- oneOf("blue","smart","crazy")
    structure <- choose(1,4)
  } yield structure match {
    case 1 => s"$subj $verb $adverb $preposition $noun"
    case 2 => s"$subj is $adjective"
    case 3 => s"$subj $verb $noun"
    case 4 => s"$preposition $noun $subj $verb"
  }

  val realisticText: Gen[String] = for {
    s1 <- presentTenseSentence
    s2 <- presentTenseSentence
    punctuation <- oneOf(".","!","?!?")
    hashtag <- oneOf("scala", "lol", "sothere")
    structure <- choose(1,3)
  } yield structure match {
    case 1 => s"$s1$punctuation"
    case 2 => s"$s1 and $s2$punctuation #$hashtag"
    case 3 => s"$s1$punctuation #$hashtag :-)"
  }

  // this tweet has no opinions yet. That's important
  val realisticTweet: Gen[IncomingTweet] = for {
    text <- realisticText
    id <- tweetId
    user <- twitterUser
  } yield IncomingTweet(TweetDetail(text, id, user))

  property("Sentences that start with I") = forAll(realisticTweet) {
    tweet =>
      val p = Process(tweet) |> iDoThisToo.opinionate()
      val output = p.toList.head.asInstanceOf[IncomingTweet]

      (output.opinions.size >= 1) ==> {
        val newOpinion = output.opinions.head // would be a better test to get the new one

        (newOpinion.points > 0) :| "Positive score" &&
        (newOpinion.hasSuggestion) :| "has a suggestion" &&
        (newOpinion.suggestedText.get.contains("I ")) :| "it's about me" &&
        (newOpinion.suggestedText.get.contains(" too!")) :| "I agree"
          // better: test that the stuff in between is also in the tweet

      }  // making no statements if this ranker never expresses an opinion
  }

  val arbitraryDouble = implicitly[Arbitrary[Double]].arbitrary suchThat { Math.abs(_) < 1000000}
  property("A ranker that learns from data coming in") = forAll(listOfN(100, realisticTweet), arbitraryDouble) {
    (incomingTweets, desiredAverageScore) =>
      val subject = iDoThisToo
      def matches(i: IncomingTweet) = subject.consider(i, RankerState()).nonEmpty
      val input = evenOut(incomingTweets, matches)
      val initialState = RankerState(targetAveragePoints = desiredAverageScore)

      val p = Process(input:_*) |> subject.opinionate(initialState) |> process1.drop(10) // reduce influence of initial state
      val output = p.toList
      val averageScore = output.map(_.asInstanceOf[IncomingTweet].totalScore).sum / output.size
      val difference = desiredAverageScore - averageScore
      val percentDifference = Math.abs(difference / averageScore)

      (difference < .01 || percentDifference < 0.2) :| s"Hoping for an average of $desiredAverageScore but was $averageScore"
  }

  // Make sure the ones that make the condition f true/false are
  // distributed about evenly through the output list.
  // All elements are included, reorder to spread f = true evenly.
  def evenOut[A](stuff: List[A], f: A => Boolean) = {
    val (larger, smaller) = stuff.partition(f) match {
      case (s, s2) if s.size > s2.size => (s, s2)
      case (s2, s) => (s, s2)
    }
    val bits = larger.grouped(larger.size / smaller.size)
    smaller.foldLeft(List.empty[A])((acc, e) => e +: (bits.next() ++ acc))
  }

  property("Easy example") = {
    val text = "I love matches!"
    val tweet = IncomingTweet(TweetDetail(text, "4", TwitterUser("me")))
    val p = Process(tweet) |> iDoThisToo.opinionate()
    val output = p.toList map {_.asInstanceOf[IncomingTweet]}

    (output.nonEmpty) :| "got something back" &&
    (output.head.opinions.nonEmpty) :| "got an opinion" &&
    (output.head.opinions.head.suggestedText.get ?= "I love matches, too!") :| "expected opinion"
  }

  property("Emits state when requested") = {
    val request = RollCall()
    val subject = iDoThisToo.opinionate()
    val output = (Process(request) |> subject).toList.map { _.asInstanceOf[RollCall]}

    (output.size ?= 1) &&
    (output.head.whosHere.size == 1)
  }

  property("Rusty's Ranker") = forAll(someIncomingTweets) {
    incomingTweets =>
      val subject = Rankers.shortIsBetter
      val p = Process(incomingTweets:_*) |> subject
      val output = p.toList map { _.asInstanceOf[IncomingTweet]}

      def round(d: Double) = (d * 100).ceil

      val inputAndOutput = incomingTweets zip output
      val tweetAndScore = inputAndOutput map { case (i, o) =>
                           (i.tweet.text.length, round(o.totalScore - i.totalScore))}
      val sortedByText = tweetAndScore sortBy (_._1)
      val sortedByPoints = tweetAndScore sortBy (_._2) reverse

      (sortedByText =? sortedByPoints) :| "Shortest text = most points"

  }


}
