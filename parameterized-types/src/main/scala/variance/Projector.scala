package variance
import scala.annotation.tailrec
import scala.util.Random

trait Converter[-S] {
  def convert(value: S): String
}

trait Slide[+R] {
  def read: (Option[R], Slide[R])
}

class Projector[R](converter: Converter[R]) {
  @tailrec
  final def project(screen: Slide[R], acc: String = ""): String = {
    screen.read match {
      case (Some(line), nextSlide) => project(nextSlide, acc + converter.convert(line))
      case (None, _)               => acc
    }
  }
}

class WordLine(val word: String)

class RedactedWordLine(val redactionFactor: Double, word: String) extends WordLine(word)

object LineConverter extends Converter[WordLine] {
  override def convert(value: WordLine): String = value.word + "\n"
}

object RedactedWordLineConverter extends Converter[RedactedWordLine] {
  override def convert(value: RedactedWordLine): String = {
    val redactedWord = value.word.map { char =>
      if (Random.nextDouble() < value.redactionFactor) '█' else char
    }.mkString
    redactedWord + "\n"
  }
}

class HelloSlide[R <: WordLine](lines: Seq[R]) extends Slide[R] {
  override def read: (Option[R], Slide[R]) = {
    if (lines.isEmpty) (None, new HelloSlide(lines))
    else (Some(lines.head), new HelloSlide(lines.tail))
  }
}
