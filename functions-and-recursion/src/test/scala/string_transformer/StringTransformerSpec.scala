package string_transformer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import string_transformer.StringTransformer._

class StringTransformerSpec extends AnyFlatSpec with Matchers {
  "double" should "return double string" in {
    val test = "Удвой меня!"
    double(test) shouldEqual "Удвой меня!Удвой меня!"
  }

  "half" should "return half string" in {
    val test = "Удвой меня!Удвой меня!"
    half(test) shouldEqual "Удвой меня!"
  }

  "half" should "return half string with rounded down length" in {
    val test = "Оппенгеймер"
    half(test) shouldEqual "Оппен"
  }

  "reverse" should "return reversed string" in {
    val test = "Довод"
    reverse(test) shouldEqual "довоД"
  }

  "transformer" should "return transformation function" in {
    val test = "Я не понимаю((("
    transformer(test)(half) shouldEqual "Я не по"
    transformer(test)(double) shouldEqual "Я не понимаю(((Я не понимаю((("
    val test2 = transformer(test)(_)
    test2(double) shouldEqual "Я не понимаю(((Я не понимаю((("
    test2(half) shouldEqual "Я не по"
    // ладно, теперь понимаю))) ( или нет((( )
  }
}
