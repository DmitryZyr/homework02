package fintech.homework02
import org.scalatest.{FlatSpec, Matchers}

class ComplexNumberSpec extends FlatSpec with Matchers {
  implicit def extendComplexNumber(i: ComplexNumber) = new ExtendedComplexNumber(i)

  "ComplexNumber" should "toString correctly" in {
    new ComplexNumber(0, 0).toString shouldEqual "0"
    new ComplexNumber(1).toString shouldEqual "1.0"
    new ComplexNumber(1, 0).toString shouldEqual "1.0"
    new ComplexNumber(1, 1).toString shouldEqual "1.0 + i"
    new ComplexNumber(1, 2).toString shouldEqual "1.0 + 2.0 * i"
    new ComplexNumber(-1, 2).toString shouldEqual "-1.0 + 2.0 * i"
    new ComplexNumber(-1, -2).toString shouldEqual "-1.0 - 2.0 * i"
    new ComplexNumber(0, -2).toString shouldEqual "-2.0 * i"
    new ComplexNumber(0.23, 2.48).toString shouldEqual "0.23 + 2.48 * i"
    new ComplexNumber(-0.23, -2.48).toString shouldEqual "-0.23 - 2.48 * i"
  }

  "ComplexNumber" should "equal correctly" in {
    new ComplexNumber(2, 2) should === (new ComplexNumber(2, 2))
    new ComplexNumber(0, 0) shouldEqual new ComplexNumber(0, 0)
    new ComplexNumber(1, 1) shouldEqual new ComplexNumber(1, 1)
    new ComplexNumber(-1, -1) shouldEqual new ComplexNumber(-1, -1)
    new ComplexNumber(1.53, 2.76) shouldEqual new ComplexNumber(1.53, 2.76)
    new ComplexNumber(-1.53, -2.76) shouldEqual new ComplexNumber(-1.53, -2.76)
  }

  "ComplexNumber" should "sum correctly" in {
    new ComplexNumber(0, 0) + new ComplexNumber(0, 0) shouldEqual new ComplexNumber(0, 0)
    new ComplexNumber(1) + new ComplexNumber(2) shouldEqual new ComplexNumber(3)
    new ComplexNumber(1, 0) + new ComplexNumber(2, 0) shouldEqual new ComplexNumber(3, 0)
    new ComplexNumber(1, 1) + new ComplexNumber(2, 2) shouldEqual new ComplexNumber(3, 3)
    new ComplexNumber(-1, -1) + new ComplexNumber(-2, -2) shouldEqual new ComplexNumber(-3, -3)
    new ComplexNumber(0, -1) + new ComplexNumber(0, -2) shouldEqual new ComplexNumber(0, -3)
    new ComplexNumber(1, -1) + new ComplexNumber(2, -2) shouldEqual new ComplexNumber(3, -3)
    new ComplexNumber(-1, 1) + new ComplexNumber(-2, 2) shouldEqual new ComplexNumber(-3, 3)
    new ComplexNumber(0.23, 2.48) + new ComplexNumber(7.12, 6.54) shouldEqualWithEps new ComplexNumber(7.35, 9.02)
    new ComplexNumber(-0.23, -2.48) + new ComplexNumber(-7.12, -6.54) shouldEqualWithEps new ComplexNumber(-7.35, -9.02)
    new ComplexNumber(0.23, -2.48) + new ComplexNumber(-7.12, 6.54) shouldEqualWithEps new ComplexNumber(-6.89, 4.06)
  }

  "ComplexNumber" should "multiply correctly" in {
    new ComplexNumber(0, 0) * new ComplexNumber(0, 0) shouldEqual new ComplexNumber(0, 0)
    new ComplexNumber(0, 0) * new ComplexNumber(0.48, 6.84) shouldEqual new ComplexNumber(0, 0)
    new ComplexNumber(3.22, -8.90) * new ComplexNumber(0, 0) shouldEqual new ComplexNumber(0, 0)
    new ComplexNumber(1) * new ComplexNumber(2) shouldEqual new ComplexNumber(2)
    new ComplexNumber(1, 0) * new ComplexNumber(2, 0) shouldEqual new ComplexNumber(2, 0)
    new ComplexNumber(1, 1) * new ComplexNumber(2, 2) shouldEqual new ComplexNumber(0, 4)
    new ComplexNumber(-1, -1) * new ComplexNumber(-2, -2) shouldEqual new ComplexNumber(0, 4)
    new ComplexNumber(0, -1) * new ComplexNumber(0, -2) shouldEqual new ComplexNumber(-2, 0)
    new ComplexNumber(1, -1) * new ComplexNumber(2, -2) shouldEqual new ComplexNumber(0, -4)
    new ComplexNumber(-1, 1) * new ComplexNumber(-2, 2) shouldEqual new ComplexNumber(0, -4)
    new ComplexNumber(0.23, 2.48) * new ComplexNumber(7.12, 6.54) shouldEqualWithEps new ComplexNumber(-14.5816, 19.1618)
    new ComplexNumber(-0.23, -2.48) * new ComplexNumber(-7.12, -6.54) shouldEqualWithEps new ComplexNumber(-14.5816, 19.1618)
    new ComplexNumber(-0.23, 2.48) * new ComplexNumber(7.12, -6.54) shouldEqualWithEps new ComplexNumber(14.5816, 19.1618)
  }
}
class ExtendedComplexNumber(value: ComplexNumber) {
  def shouldEqualWithEps(expected: ComplexNumber, eps: Double = 0.0001) = {
    assert(math.abs(value.re - expected.re) < eps, s"eps $eps was exceeded. value: ${value.re}, expected: ${expected.re}")
    assert(math.abs(value.im - expected.im) < eps, s"eps $eps was exceeded. value: ${value.im}, expected: ${expected.im}")
  }
}
