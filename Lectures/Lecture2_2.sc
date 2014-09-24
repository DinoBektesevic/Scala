package week02

object Lecture2_2 {
  def Ident(x: Int): Int = x                      //> Ident: (x: Int)Int
	def Sqr(x: Int): Int = x*x                //> Sqr: (x: Int)Int
	def Cube(x: Int): Int = x*x*x             //> Cube: (x: Int)Int

	//Currying is when you have a function that returns
	//another function that takes parameters.
	//Benefits are that return of first function, a function,
	//can be reused again in another aspect, i.e. as sorting "filter"
	
	def sum(f:Int => Int): (Int, Int) =>Int = {
		def SumF(a: Int, b: Int): Int = {
			if (a>b) 0
			else f(a)+SumF(a+1, b)
		}
		SumF
	}                                         //> sum: (f: Int => Int)(Int, Int) => Int
	
	sum(Ident)                                //> res0: (Int, Int) => Int = <function2>
	sum(Sqr)                                  //> res1: (Int, Int) => Int = <function2>
	sum(Cube)                                 //> res2: (Int, Int) => Int = <function2>
	
	//But also we can send parameters: (Int, Int) => Int
	//thing that's the Sum return type
	
	sum(Ident)(1, 3)                          //> res3: Int = 6
	sum(Sqr)(1, 3)                            //> res4: Int = 14
	sum(Cube)(1, 3)                           //> res5: Int = 36
	sum(x=>x*x*x)(1, 3)                       //> res6: Int = 36
	
	//Or we can still redefine each func. especially
	
	def Sum(a: Int, b: Int): Int =  sum(Ident)(a, b)
                                                  //> Sum: (a: Int, b: Int)Int
  def SumSqr(a: Int, b: Int): Int =  sum(Sqr)(a, b)
                                                  //> SumSqr: (a: Int, b: Int)Int
  def SumCube(a: Int, b: Int): Int = sum(Cube)(a, b)
                                                  //> SumCube: (a: Int, b: Int)Int
	
	//Or again because we're using anonymous functions
	//we can just pass func directly to the sum:
	def ASum(a: Int, b: Int): Int =  sum(x=>x)(a, b)
                                                  //> ASum: (a: Int, b: Int)Int
  def ASumSqr(a: Int, b: Int): Int =  sum(x=>x*x)(a, b)
                                                  //> ASumSqr: (a: Int, b: Int)Int
  def ASumCube(a: Int, b: Int): Int = sum(x=>x*x*x)(a, b)
                                                  //> ASumCube: (a: Int, b: Int)Int
	
	ASumCube(1, 3)                            //> res7: Int = 36
	SumCube(1, 3)                             //> res8: Int = 36
	
	//It is possible to shorten the original sum function
	//by just cascading aditional () after its definiton
	//Difference between the above one is that you need to follow
	//it with a _, when trying to call only the "outter" func.
	
	def ShortSum(f: Int => Int)(a: Int, b: Int): Int = {
		if (a>b) 0 else f(a)+ShortSum(f)(a+1, b)
	}                                         //> ShortSum: (f: Int => Int)(a: Int, b: Int)Int
	
	ShortSum(Cube)(1, 3)                      //> res9: Int = 36
	ShortSum(Cube)_                           //> res10: (Int, Int) => Int = <function2>
	

	//1. Write a product function
	//2. Write a factorial in terms of products
	//3. Write a more general func. that calcs sum and product
	
	def Product(f: Int => Int)(a: Int, b: Int): Int = {
		if (a>b) 1 else f(a)*Product(f)(a+1, b)
	}                                         //> Product: (f: Int => Int)(a: Int, b: Int)Int
	
	Product(Ident)(1,5)                       //> res11: Int = 120
	Product(x=>x*x)(3,4)                      //> res12: Int = 144
	9*16                                      //> res13: Int(144) = 144
	
	def factorial(n:Int)= Product(x=>x)(1,n)  //> factorial: (n: Int)Int
	factorial(5)                              //> res14: Int = 120
	
	//3. Sum and Product have 3 things written into them.
	//1st is the function we're summing/multiplying (squares, identities, cubes)
	//2nd is the way we combine the former with next value in recursion: f(a) * or + recurse(a+1, b)
	//3rd is the zero value, for Sum it's 0, for product 1
	def mapReduce(f: Int=>Int, combine:(Int, Int) => Int, zero: Int)(a:Int, b:Int): Int = {
		if (a>b) zero
		else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
	}                                         //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b
                                                  //| : Int)Int
  //rewrite sum and prod:
  def mapSum(a: Int, b: Int): Int = mapReduce(Ident, (x,y)=>x+y, zero=0)(a, b)
                                                  //> mapSum: (a: Int, b: Int)Int
  def mapProduct(a: Int, b: Int): Int = mapReduce(Ident, (x,y)=>x*y, zero=1)(a,b)
                                                  //> mapProduct: (a: Int, b: Int)Int
  
  mapSum(1,3)                                     //> res15: Int = 6
  mapProduct(1,3)                                 //> res16: Int = 6
  mapSum(1,5)                                     //> res17: Int = 15
  mapProduct(1,5)                                 //> res18: Int = 120
  
  
  
  
}