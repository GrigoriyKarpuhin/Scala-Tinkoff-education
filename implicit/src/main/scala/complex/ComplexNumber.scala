package complex
import scala.language.implicitConversions
import scala.math.{atan2, sqrt}

final case class ComplexNumber(real: Double, imaginary: Double) {
  def *(other: ComplexNumber) =
    ComplexNumber(
      (real * other.real) + (imaginary * other.imaginary),
      (real * other.imaginary) + (imaginary * other.real)
    )
  def +(other: ComplexNumber) =
    ComplexNumber(real + other.real, imaginary + other.imaginary)
  def ~=(o: ComplexNumber) =
    (real - o.real).abs < 1e-6 && (imaginary - o.imaginary).abs < 1e-6
}

object ComplexNumber {
  implicit class ComplexOther(val complex: ComplexNumber) {
    def -(other: ComplexNumber): ComplexNumber = {
      ComplexNumber(complex.real - other.real, complex.imaginary - other.imaginary)
    }

    def /(other: ComplexNumber): ComplexNumber = {
      val denominator = other.real * other.real + other.imaginary * other.imaginary

      if (denominator == 0) {
        throw new ArithmeticException("Division by zero")
      }

      val numeratorReal = complex.real * other.real + complex.imaginary * other.imaginary
      val numeratorImaginary = complex.imaginary * other.real - complex.real * other.imaginary
      ComplexNumber(numeratorReal / denominator, numeratorImaginary / denominator)
    }

    def polarForm: (Double, Double) = {
      if (complex.real == 0) {
        throw new ArithmeticException("Division by zero")
      }
      val rad = sqrt(complex.real * complex.real + complex.imaginary * complex.imaginary)
      val ang = atan2(complex.imaginary, complex.real)
      (rad, ang)
    }
  }

  implicit def fromNumeric[T](value: T)(implicit num: Numeric[T]): ComplexNumber = {
    ComplexNumber(num.toDouble(value), 0)
  }
  implicit def fromNumericOther[T: Numeric](value: T): ComplexOther = {
    ComplexOther(ComplexNumber(Numeric[T].toDouble(value), 0))
  }

  implicit class ComplexSyntax(value: Double) {
    def i: ComplexNumber = {
      ComplexNumber(0.0, value)
    }
  }
}
