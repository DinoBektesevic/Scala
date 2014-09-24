package week02

object Lecture2_3 {
	import scala.math
	//Find a fixed point of a function f(x)=1+x/2
	//a Fixed point is a point for which f(x) = x
	//some fixed points can be found by f(x), f(f(x)), f(f(f(x)))

	val error = 1e-6                          //> error  : Double = 1.0E-6
	def isGoodEnough(x: Double, y: Double): Boolean = {
		math.abs((x-y)/x)/x < error
	}                                         //> isGoodEnough: (x: Double, y: Double)Boolean
	
	def FindFP(f:Double => Double)(firstGuess: Double): Double = {
		def loop(x: Double): Double = {
			if (isGoodEnough(x, f(x)))  f(x)
			else loop(f(x))
		}
		loop(firstGuess)
		
	}                                         //> FindFP: (f: Double => Double)(firstGuess: Double)Double
	
  FindFP(x=>1.0+x/2.0)(1.0)                       //> res0: Double = 1.9999961853027344
  
  //square root is a fixed point of y = x/y
  //because when y = x/y we've got a square root of x
  //but we have to average it because square root grows
  //too fast and that makes FindFP oscilate between 1.0 and 2.0
  def sqrt(x: Double) = FindFP(y=>(y+x/y)/2)(1.0) //> sqrt: (x: Double)Double
  
  sqrt(2.0)                                       //> res1: Double = 1.414213562373095
  
  //we can extract the average damping trick to separate func.
  def averageDamp(f: Double => Double)(x: Double) = (x+f(x))/2
                                                  //> averageDamp: (f: Double => Double)(x: Double)Double
  
  def Dsqrt(x: Double) = FindFP(averageDamp(y=>x/y))(x)
                                                  //> Dsqrt: (x: Double)Double
  
  Dsqrt(2.0)                                      //> res2: Double = 1.414213562373095
    
}