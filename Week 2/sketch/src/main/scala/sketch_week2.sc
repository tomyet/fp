class Rational(x: Int, y: Int) {

  require(y != 0, "denominator must be non-zero")

  def numer = x
  def denom = y

  def this(x: Int) = this(x, 1)

  override def toString = x + "/" + y

  def +(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
}


val half = new Rational(1, 2)

val threeQuarters = half + new Rational(1, 4)

val one = new Rational(1)

val bad = new Rational(1, 0)