package week02

object Lecture2_1 {

	
	def Ident(x: Int): Int = x                //> Ident: (x: Int)Int
	def Sqr(x: Int): Int = x*x                //> Sqr: (x: Int)Int
	def Cube(x: Int): Int = x*x*x             //> Cube: (x: Int)Int

	def SumInts(a: Int, b: Int): Int = {
		if (a>b) 0 else SumInts(a+1, b)
	}                                         //> SumInts: (a: Int, b: Int)Int
	def SumSqrs(a: Int, b: Int): Int = {
		if (a>b) 0 else Sqr(a)+SumSqrs(a+1, b)
	}                                         //> SumSqrs: (a: Int, b: Int)Int
	def SumCubes(a: Int, b: Int): Int = {
		if (a>b) 0 else Cube(a)+SumCubes(a+1, b)
	}                                         //> SumCubes: (a: Int, b: Int)Int
	
	//a better way is to try and get rid of the
  //need to define each func. separately (Ident, and then Sum)
  //and have the user define whatever func he needs
  //enter, anonymouse func.

  def sum(f: Int => Int, a: Int, b: Int): Int ={
  	if (a>b) 0
  	else f(a)+sum(f, a+1, b)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int
  
  sum(Ident, 1, 3)                                //> res0: Int = 6
  sum(Sqr, 1, 3)                                  //> res1: Int = 14
  sum(Cube, 1, 3)                                 //> res2: Int = 36
  
  //or define:
  //because calls to them will be shorter
  
  def Sum(a: Int, b: Int) =  sum(Ident, a, b)     //> Sum: (a: Int, b: Int)Int
  def SumSqr(a: Int, b: Int) =  sum(Sqr, a, b)    //> SumSqr: (a: Int, b: Int)Int
  def SumCube(a: Int, b: Int) = sum(Cube, a, b)   //> SumCube: (a: Int, b: Int)Int
  
  Sum(1, 3)                                       //> res3: Int = 6
  SumSqr(1, 3)                                    //> res4: Int = 14
  SumCube(1, 3)                                   //> res5: Int = 36
  
  
  //Tail recursion benefits are that for large intervals
  //we avoid Stack frame overflows
	  
  def TailRecSum(f: Int => Int, a: Int, b: Double): Int ={
  	def loop(a: Int, acc: Int): Int = {
  		if (a>b) acc
  		else loop(a+1, f(a)+acc)
  	}
  	loop(a, 0)
  }                                               //> TailRecSum: (f: Int => Int, a: Int, b: Double)Int
  
  TailRecSum(Ident, 0, 1e8)                       //> res6: Int = 987459712
  
  
  
}