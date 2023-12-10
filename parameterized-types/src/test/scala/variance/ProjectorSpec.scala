package variance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProjectorSpec extends AnyFlatSpec with Matchers {
  "converter" should "hide lines" in {
    val test = new RedactedWordLine(1, "Евгений Сергей стол лампа")
    RedactedWordLineConverter.convert(test) shouldEqual ("█████████████████████████" + "\n")
    val test1 = new RedactedWordLine(1, "Евгений")
    val test2 = new RedactedWordLine(0, "Сергей")
    val test3 = new RedactedWordLine(0, "стол")
    val test4 = new RedactedWordLine(1, "лампа")
    val t1 = RedactedWordLineConverter.convert(test1)
    val t2 = RedactedWordLineConverter.convert(test2)
    val t3 = RedactedWordLineConverter.convert(test3)
    val t4 = RedactedWordLineConverter.convert(test4)
    (t1 + t2 + t3 + t4) shouldEqual ("███████\nСергей\nстол\n█████\n")
  }

  "helloslide" should "show slides" in {
    val test = new HelloSlide[WordLine](Seq(new WordLine("было"), new WordLine("umbrella"), new WordLine("cucumber")))
    val (Some(h1), t1) = test.read
    val (Some(h2), t2) = t1.read
    val (Some(h3), t3) = t2.read
    h1.word shouldEqual ("было")
    h2.word shouldEqual ("umbrella")
    h3.word shouldEqual ("cucumber")
    t3.read._1 shouldEqual (None)
  }

  "slide and convertor" should "correctly work with types" in {
    "new Projector[WordLine](LineConverter)" should compile
    "new Projector[WordLine](RedactedWordLineConverter)" shouldNot typeCheck
    val projector1 = new Projector[WordLine](LineConverter)
    val projector2 = new Projector[RedactedWordLine](RedactedWordLineConverter)
    """projector1.project(new HelloSlide[WordLine](Seq(new WordLine("было"),new WordLine("umbrella"),new WordLine("cucumber"))))""" should compile
    """projector1.project(new HelloSlide[RedactedWordLine](Seq(new RedactedWordLine(0.5,"было"),new RedactedWordLine(0.5, "umbrella"),new RedactedWordLine(0.5, "cucumber"))))""" should compile
    """projector2.project(new HelloSlide[RedactedWordLine](Seq(new RedactedWordLine(0.5,"было"),new RedactedWordLine(0.5, "umbrella"),new RedactedWordLine(0.5, "cucumber"))))""" should compile
    """projector2.project(new HelloSlide[WordLine](Seq(new WordLine("было"),new WordLine("umbrella"),new WordLine("cucumber"))))""" shouldNot typeCheck
  }
}
