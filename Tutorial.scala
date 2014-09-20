import scala.io.Source

/**
 * Created by Dino on 8.9.2014..
 */
object Second {


  def readlines(filename: String): Unit = {
    import scala.io.Source

    if (filename.length > 0) {
      for (line <- Source.fromFile(filename).getLines)
        println(line.length + " " + line)
    }
    else
      Console.err.println("Please enter filename")
  }


  def improve(guess: Double, x: Double): Double = {
    println((guess +x/guess)/2)
    (guess +x/guess)/2
  }                                         //> improve: (guess: Double, x: Double)Double

  def isGoodEnough(guess: Double, x: Double): Boolean = {
    (guess*guess > x-0.001) && (guess*guess < x+0.001)
  }                                         //> isGoodEnough: (guess: Double, x: Double)Boolean

  def sqrtIter(guess: Double, x: Double): Double = {
    if(isGoodEnough(guess, x)) guess else sqrtIter(improve(guess, x), x)
  }                                         //> sqrtIter: (guess: Double, x: Double)Double

  def sqrt(x: Double): Double ={
    sqrtIter(1.0, x)
  }                                         //> sqrt: (x: Double)Double

  def main(args: Array[String]) {
    //readlines("C:\\Users\\Dino\\Desktop\\SCALA\\src\\Second.scala")

    println(sqrt(2))
  }
}
