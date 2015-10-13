package problem050

object problem050 {
	// prob001
  (1 until 1000).filter(x => (x%3==0 || x%5==0)).sum
                                                  //> res0: Int = 233168
  
  // prob002
  def sumOfFibEven(prev: Int, cur:Int, acc:Int): Int = cur match {
  	case x if (x<=4000000 && x%2==0) => sumOfFibEven(cur, prev+cur, acc+cur)
  	case x if (x<=4000000) => sumOfFibEven(cur, prev+cur, acc)
  	case _ => acc
  }                                               //> sumOfFibEven: (prev: Int, cur: Int, acc: Int)Int
  sumOfFibEven(1, 2, 0)                           //> res1: Int = 4613732
}