import week03._

object Lecture3_3 {
	def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
                                                  //> singleton: [T](elem: T)week03.Cons[T]

	singleton[Int](1)                         //> res0: week03.Cons[Int] = week03.Cons@1e384de
	singleton(1)                              //> res1: week03.Cons[Int] = week03.Cons@280bca

	def nth[T](n: Int, xs: List[T]): T =
		if (xs.isEmpty) throw new IndexOutOfBoundsException
		else if (n==0) xs.head
		else nth(n-1, xs.tail)            //> nth: [T](n: Int, xs: week03.List[T])T
		
	val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
                                                  //> list  : week03.Cons[Int] = week03.Cons@11dfada
	
	nth(1, list)                              //> res2: Int = 2
}