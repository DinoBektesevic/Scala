package week01

object Lecture1_6 {

  def sqrt(x: Double): Double = {
    def abs(y: Double) = if (y < 0) -y else y

    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    def isGoodEnough(guess: Double) = {
      abs(guess * guess - x) / x < 1e-6
    }

    def improve(guess: Double) = {
      (guess + x / guess) / 2
    }

    return sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double

  sqrt(2.0)                                       //> res0: Double = 1.4142135623746899
  sqrt(1e-20)                                     //> res1: Double = 1.000000000002308E-10
  sqrt(1e60)                                      //> res2: Double = 1.0000000031080746E30

}