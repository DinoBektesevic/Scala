package week02

object Lecture2_6 {
	class Rational(x: Int, y: Int ) {
		//safeguard against x/0 which doesn't exist
		//require is for conditions you know in advance
		//to check if i.e. a squared number is positive,
		//that is a condition that can only be checked after
		//code executed, use assert
		//require --> pre-conditions, assert --> code functionality
		require(y > 0, "denominator must be nonzero!")
		//greatest common denominator to simplify the rational
		private def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
		private val g = gcd(x, y)
		//UNLIKE in Lecture2_5 it's better
		//to have numer and denom as vals, since vals are only calculated once
		//and not every time like with def
		val numer = x/g
		val denom = y/g
		
		//let's add another constructor
		//takes in 1 number, returns x/1 rational
		def this(x: Int) = this(x, 1)
		
		def add(r: Rational) ={
	  	new Rational(
	  		numer*r.denom +denom*r.numer,
	  		denom*r.denom)
		  }
		  
		  def neg: Rational = new Rational(-numer, denom)
	
		  def sub(r: Rational) = add(r.neg)
		  
		  def div(r: Rational) ={
		  	new Rational(
		  		numer*r.denom,
		  		denom*r.numer)
		  }
		  
		  def less(r: Rational) = numer*r.denom<r.numer*denom
		  
		  def max(that: Rational) = if (this.less(that)) that else this
			
			override def toString = numer +"/"+denom
			//TASK: don't simplify internally, simplify on output
			// override def toString = numer/g +"/"+denom/g
		}

  val x = new Rational(1,2)                       //> x  : week02.Lecture2_6.Rational = 1/2
	val y = new Rational(5,7)                 //> y  : week02.Lecture2_6.Rational = 5/7
	val z = new Rational(3,2)                 //> z  : week02.Lecture2_6.Rational = 3/2
	
	x.add(y).add(z)                           //> res0: week02.Lecture2_6.Rational = 19/7
	
	x.sub(x)                                  //> java.lang.IllegalArgumentException: requirement failed: denominator must be
                                                  //|  nonzero!
                                                  //| 	at scala.Predef$.require(Predef.scala:219)
                                                  //| 	at week02.Lecture2_6$Rational.<init>(week02.Lecture2_6.scala:11)
                                                  //| 	at week02.Lecture2_6$Rational.add(week02.Lecture2_6.scala:26)
                                                  //| 	at week02.Lecture2_6$Rational.sub(week02.Lecture2_6.scala:33)
                                                  //| 	at week02.Lecture2_6$$anonfun$main$1.apply$mcV$sp(week02.Lecture2_6.scal
                                                  //| a:56)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at week02.Lecture2_6$.main(week02.Lecture2_6.scala:48)
                                                  //| 	at week02.Lecture2_6.main(week02.Lecture2_6.scala)
	
	x.max(z)
	
	new Rational(2)
}