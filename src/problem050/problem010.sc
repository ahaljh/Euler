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
  def getPrimeFactor(num: Long, startNum:Int, maxPrimeFactor:Int ): Int = num match {
  	case 1 => maxPrimeFactor
  	case n if (n%startNum==0) => getPrimeFactor(num/startNum, startNum, startNum)
  	case n => getPrimeFactor(num, startNum+1, maxPrimeFactor)
  }                                               //> getPrimeFactor: (num: Long, startNum: Int, maxPrimeFactor: Int)Int
  getPrimeFactor(600851475143L, 2, 1)             //> res2: Int = 6857
  
  // prob004
  def isPalindrome(number: Int): Boolean = {
  	val str = number.toString
  	str.equals(str.reverse)
  }                                               //> isPalindrome: (number: Int)Boolean
  (100 to 999).flatMap(x => (x to 999).map(y => x*y)).filter(isPalindrome).max
                                                  //> res3: Int = 906609
  // prob005
  def lcm(a: Long, b: Long): Long = {
  	def gcd(a:Long, b:Long): Long = {
  		if (b==0) a
  		else gcd(b, a%b)
  	}
  	a * b / gcd(a,b)
  }                                               //> lcm: (a: Long, b: Long)Long
  (1L to 20L).reduce(lcm(_,_))                    //> res4: Long = 232792560
  
  // prob006
  math.pow((1 to 100).sum, 2).toLong - (1 to 100).map(math.pow(_, 2)).sum.toLong
                                                  //> res5: Long = 25164150

  // prob007
  def isPrime(number: Int): Boolean = {
    for (i <- (2 to math.sqrt(number).toInt)) {
      if (number%i==0) return false
    }
    true
  }                                               //> isPrime: (number: Int)Boolean
  
  def getNthPrime(nth: Int, curNum: Int=2): Int = {
    if (isPrime(curNum)) {
      if (nth==1) curNum
      else getNthPrime(nth-1, curNum+1)
    } else {
    	getNthPrime(nth, curNum+1)
    }
  }                                               //> getNthPrime: (nth: Int, curNum: Int)Int
  getNthPrime(10001)                              //> res6: Int = 104743

	// prob008
	val numberstr: String = "73167176531330624919225119674426574742355349194934"+
                          "96983520312774506326239578318016984801869478851843"+
                          "85861560789112949495459501737958331952853208805511"+
                          "12540698747158523863050715693290963295227443043557"+
                          "66896648950445244523161731856403098711121722383113"+
                          "62229893423380308135336276614282806444486645238749"+
                          "30358907296290491560440772390713810515859307960866"+
                          "70172427121883998797908792274921901699720888093776"+
                          "65727333001053367881220235421809751254540594752243"+
                          "52584907711670556013604839586446706324415722155397"+
                          "53697817977846174064955149290862569321978468622482"+
                          "83972241375657056057490261407972968652414535100474"+
                          "82166370484403199890008895243450658541227588666881"+
                          "16427171479924442928230863465674813919123162824586"+
                          "17866458359124566529476545682848912883142607690042"+
                          "24219022671055626321111109370544217506941658960408"+
                          "07198403850962455444362981230987879927244284909188"+
                          "84580156166097919133875499200524063689912560717606"+
                          "05886116467109405077541002256983155200055935729725"+
                          "71636269561882670428252483600823257530420752963450"
                                                  //> numberstr  : String = 73167176531330624919225119674426574742355349194934969
                                                  //| 835203127745063262395783180169848018694788518438586156078911294949545950173
                                                  //| 795833195285320880551112540698747158523863050715693290963295227443043557668
                                                  //| 966489504452445231617318564030987111217223831136222989342338030813533627661
                                                  //| 428280644448664523874930358907296290491560440772390713810515859307960866701
                                                  //| 724271218839987979087922749219016997208880937766572733300105336788122023542
                                                  //| 180975125454059475224352584907711670556013604839586446706324415722155397536
                                                  //| 978179778461740649551492908625693219784686224828397224137565705605749026140
                                                  //| 797296865241453510047482166370484403199890008895243450658541227588666881164
                                                  //| 271714799244429282308634656748139191231628245861786645835912456652947654568
                                                  //| 284891288314260769004224219022671055626321111109370544217506941658960408071
                                                  //| 984038509624554443629812309878799272442849091888458015616609791913387549920
                                                  //| 05240636899125607176060
                                                  //| Output exceeds cutoff limit.
  val arr: Seq[Int] = numberstr.toArray.map(c => c-'0')
                                                  //> arr  : Seq[Int] = ArraySeq(7, 3, 1, 6, 7, 1, 7, 6, 5, 3, 1, 3, 3, 0, 6, 2, 
                                                  //| 4, 9, 1, 9, 2, 2, 5, 1, 1, 9, 6, 7, 4, 4, 2, 6, 5, 7, 4, 7, 4, 2, 3, 5, 5, 
                                                  //| 3, 4, 9, 1, 9, 4, 9, 3, 4, 9, 6, 9, 8, 3, 5, 2, 0, 3, 1, 2, 7, 7, 4, 5, 0, 
                                                  //| 6, 3, 2, 6, 2, 3, 9, 5, 7, 8, 3, 1, 8, 0, 1, 6, 9, 8, 4, 8, 0, 1, 8, 6, 9, 
                                                  //| 4, 7, 8, 8, 5, 1, 8, 4, 3, 8, 5, 8, 6, 1, 5, 6, 0, 7, 8, 9, 1, 1, 2, 9, 4, 
                                                  //| 9, 4, 9, 5, 4, 5, 9, 5, 0, 1, 7, 3, 7, 9, 5, 8, 3, 3, 1, 9, 5, 2, 8, 5, 3, 
                                                  //| 2, 0, 8, 8, 0, 5, 5, 1, 1, 1, 2, 5, 4, 0, 6, 9, 8, 7, 4, 7, 1, 5, 8, 5, 2, 
                                                  //| 3, 8, 6, 3, 0, 5, 0, 7, 1, 5, 6, 9, 3, 2, 9, 0, 9, 6, 3, 2, 9, 5, 2, 2, 7, 
                                                  //| 4, 4, 3, 0, 4, 3, 5, 5, 7, 6, 6, 8, 9, 6, 6, 4, 8, 9, 5, 0, 4, 4, 5, 2, 4, 
                                                  //| 4, 5, 2, 3, 1, 6, 1, 7, 3, 1, 8, 5, 6, 4, 0, 3, 0, 9, 8, 7, 1, 1, 1, 2, 1, 
                                                  //| 7, 2, 2, 3, 8, 3, 1, 1, 3, 6, 2, 2, 2, 9, 8, 9, 3, 4, 2, 3, 3, 8, 0, 3, 0, 
                                                  //| 8, 1, 3, 5, 3, 3, 6, 2, 7, 6, 6, 1, 4, 2, 8, 2, 8, 0, 6, 4, 4, 4, 4, 8, 6, 
                                                  //| 6, 4, 5, 2, 3, 8, 7, 4,
                                                  //| Output exceeds cutoff limit.
  
  (0 until 1000-5).map(i => arr.slice(i, i+5)).map(ia => ia.reduce(_*_)).max
                                                  //> res7: Int = 40824
                                                  
  // prob009
  def isPythagorian(a: Int, b: Int, c: Int): Boolean =
    if (a*a + b*b == c*c) true else false         //> isPythagorian: (a: Int, b: Int, c: Int)Boolean
  
  def findNumber(): Int = {
    for (a <- (1 to 332)) {
      for (b <- (a+1 to 499)) {
        if (b<1000-a-b && isPythagorian(a, b, 1000-a-b)) return a*b*(1000-a-b)
      }
    }
    0
  }                                               //> findNumber: ()Int
  findNumber()                                    //> res8: Int = 31875000
  
  // prob010
  (for (i <- (2 to 2000000) if (isPrime(i))) yield(i)).map(x => x.toLong).sum
                                                  //> res9: Long = 142913828922
  
}