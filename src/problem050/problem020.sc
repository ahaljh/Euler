package problem050

object problem020 {

  // prob014
  def collatz(number: Long, acc: Int=0): Int = number match {
  	case 1 => acc+1
  	case n if n%2==0 => collatz(n/2, acc+1)
  	case n => collatz(3*n+1, acc+1)
  }                                               //> collatz: (number: Long, acc: Int)Int

	(1 to 1000000).map(x => (x, collatz(x))).foldLeft((0,0))((x,y) => if (x._2 > y._2) x else y)._1
                                                  //> res0: Int = 837799
	
	
	// prob015
	def factorial(number: Int, acc: BigInt = 1): BigInt =
		if (number > 1) factorial(number-1, acc*number) else acc
                                                  //> factorial: (number: Int, acc: BigInt)BigInt
	
	factorial(40) / (factorial(20) * factorial(20))
                                                  //> res1: scala.math.BigInt = 137846528820
  
  // prob016
  def exp(number: Int, expNum: Int, acc: BigInt=1): BigInt =
    if (expNum > 1) exp(number, expNum-1, acc*number) else acc
                                                  //> exp: (number: Int, expNum: Int, acc: BigInt)BigInt
    
  exp(2, 1000, 1).toString.toCharArray().map((x:Char) => x.toString.toInt).sum
                                                  //> res2: Int = 1367
                                                  
  // prob017
  def numOfChar(number: Int): Int = number match {
    case 1 | 2 | 6 | 10 => 3
    case 4 | 5 | 9 => 4
    case 3 | 7 | 8 | 40 | 50 | 60  => 5
    case 11 | 12 | 20 | 30 | 80 | 90  => 6
    case 15 | 16 | 70  => 7
    case 13 | 14 | 18 | 19 => 8
    case 17 => 9
    case 1000 => 11
    case n if (n >= 100) => numOfChar(n/100)+7+ (if(n%100>0) numOfChar(n%100) + 3 else 0)
    case n => numOfChar(n-(n%10)) + numOfChar(n%10)
  }                                               //> numOfChar: (number: Int)Int
  
  (1 to 1000).map(x => numOfChar(x)).sum          //> res3: Int = 21124
}