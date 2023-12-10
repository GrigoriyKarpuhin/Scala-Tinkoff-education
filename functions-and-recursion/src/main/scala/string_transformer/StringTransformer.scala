package string_transformer

object StringTransformer {
  def double: String => String = str => str + str

  def half: String => String = str => str.substring(0, str.length / 2)

  def reverse: String => String = str => str.reverse

  def transformer(str: String)(f: String => String): String = f(str)
}
