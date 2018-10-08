package fintech.homework02

class ComplexNumber (val re: Double, val im: Double = 0.0) {

  def +(c: ComplexNumber): ComplexNumber = new ComplexNumber(re + c.re, im + c.im)

  def *(c: ComplexNumber): ComplexNumber = new ComplexNumber(re * c.re - im * c.im, re * c.im + im * c.re)

  def ~(r: Double): ComplexNumber = {
    val (rad, ang) = polar
    new ComplexNumber(math.pow(rad, r), ang * r)
  }

  def radius: Double = math.sqrt(math.pow(re, 2) + math.pow(im, 2))

  def polar: (Double, Double) = {
    val rad = radius
    (rad, Math.acos(re / rad))
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[ComplexNumber]

  override def equals(other: Any): Boolean = other match {
    case that: ComplexNumber =>
      (that canEqual this) &&
        re == that.re &&
        im == that.im
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(re, im)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = {
    val str = if (re == 0 && im == 0) "0"
    else if (re == 0) im + " * i"
    else if (im == 0) re.toString
    else if (im == 1) re + " + i"
    else if (im == -1) re + " - i"
    else re + (if (im < 0) " - " + -im else " + " + im) + " * i"
    str
  }
}