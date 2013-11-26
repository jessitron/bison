package jessitron.tweetbot

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

  val realisticTweet: Gen[IncomingTweet] = realisticText map {t => IncomingTweet(TweetDetail(t))}

  property("Sentences that start with I") = forAll(realisticTweet) {
    tweet =>
      val p = Process(tweet) |> Rankers.iDoThisToo
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

  property("Easy example") = {
    val text = "I love matches!"
    val tweet = IncomingTweet(TweetDetail(text))
    val p = Process(tweet) |> Rankers.iDoThisToo
    val output = p.toList map {_.asInstanceOf[IncomingTweet]}

    (output.nonEmpty) :| "got something back" &&
    (output.head.opinions.nonEmpty) :| "got an opinion" &&
    (output.head.opinions.head.suggestedText.get ?= "I love matches, too!") :| "expected opinion"
  }


}
