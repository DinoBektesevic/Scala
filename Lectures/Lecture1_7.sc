package week01

object Lecture1_7 {
  def factorial(x: Int): Int = {
  	def loop(acc: Int, x: Int): Int = {
  		if (x==0) return acc
  		else loop(acc*x, x-1)
  		}
  	loop(1, x)
  }                                               //> factorial: (x: Int)Int
  
  factorial(3)                                    //> res0: Int = 6
}