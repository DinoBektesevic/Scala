package week02

object Lecture2_7 {
  	class Rational(x: Int, y: Int ) {
			require(y > 0, "denominator must be nonzero!")
			
			private def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
			private val g = gcd(x, y)
			val numer = x/g
			val denom = y/g
			
			def this(x: Int) = this(x, 1)

		  def +(r: Rational) = new Rational(numer*r.denom +denom*r.numer,
		  																	denom*r.denom)
		  
		  //unary_- denotes a negative - and not a subtraction operation
		  def unary_- : Rational = new Rational(-numer, denom)
	
		  def -(that: Rational) = this + -that
		  
		  //why not use the < sign? because < is an identifier we can!
		  def < (r: Rational) = numer*r.denom<r.numer*denom
		  
		  def max(that: Rational) = if (this < that) that else this
			
			override def toString = numer +"/"+denom
		}
		
		val x = new Rational(1,2)         //> x  : week02.Lecture2_7.Rational = 1/2
		val y = new Rational(5,7)         //> y  : week02.Lecture2_7.Rational = 5/7
		val z = new Rational(3,2)         //> z  : week02.Lecture2_7.Rational = 3/2
		
		//INFIX methods/func. are those funcs that take a parameter
		//and can be used both as:
		x.max(y)                          //> res0: week02.Lecture2_7.Rational = 5/7
		x max y                           //> res1: week02.Lecture2_7.Rational = 5/7

		//add/sub can be replaced easily:
		x+x                               //> res2: week02.Lecture2_7.Rational = 1/1
		//x+x-y
}