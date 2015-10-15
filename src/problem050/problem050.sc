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
  
  // prob003
  def getPrimeFactor(num: Long, startNum:Int, maxPrimeFactor:Int ): Int = {
		if (num%startNum == 0) getPrimeFactor(num/startNum, startNum, startNum)
		else if (num == 1) maxPrimeFactor
		else getPrimeFactor(num, startNum+1, maxPrimeFactor)
  }                                               //> getPrimeFactor: (num: Long, startNum: Int, maxPrimeFactor: Int)Int
  
  getPrimeFactor(600851475143L, 2, 1)             //> res2: Int = 6857
  
  // prob004
  def isPalindrome(number: Int): Boolean = {
  	val str = number.toString
  	str.equals(str.reverse)
  }                                               //> isPalindrome: (number: Int)Boolean
  
  (100 to 999).flatMap(x => (x to 999).map(y => x*y)).filter(isPalindrome).max
                                                  //> res3: Int = 906609
  
 
}