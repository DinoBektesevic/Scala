package week02

object Lecture2_5 {
  new Rational(1,2)                               //> res0: week02.Rational = 1/2
  
  val x = new Rational(1,2)                       //> x  : week02.Rational = 1/2
  x.numer                                         //> res1: Int = 1
  x.denom                                         //> res2: Int = 2
  
  def addRat(x: Rational, y: Rational): Rational ={
  	new Rational(x.numer*y.denom +x.denom*y.numer, x.denom*y.denom)
  }                                               //> addRat: (x: week02.Rational, y: week02.Rational)week02.Rational
  
  //bad output, can't see what's what
  //NOW NOT TRUE BECAUSE OF ADDED CLASS METHODS
  addRat(x,x)                                     //> res3: week02.Rational = 4/4
  
  
  //make them print out nicely
  def makeString(r: Rational): String ={
  	r.numer +"/"+r.denom
  }                                               //> makeString: (r: week02.Rational)String
   
  makeString(addRat(x,x))                         //> res4: String = 4/4
  
  
  //A BETTER IDEA IS TO IMPLEMENT THESE FUNCS AS METHODS OF THE CLASS
	x.add(x)                                  //> res5: week02.Rational = 4/4
	
	val ex = new Rational(1,3)                //> ex  : week02.Rational = 1/3
	val y = new Rational(5,7)                 //> y  : week02.Rational = 5/7
	val z = new Rational(3,2)                 //> z  : week02.Rational = 3/2
	
	ex.sub(y).sub(z)                          //> res6: week02.Rational = -79/42
	
	
}

class Rational(x: Int, y: Int ) {
	def numer = x
	def denom = y
	
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
	
	override def toString = numer +"/"+denom
	
	
}