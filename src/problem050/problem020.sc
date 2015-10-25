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
  
  // prob018
  val strInput =
                 "75\n" +
                 "95 64\n" +
                 "17 47 82\n" +
                 "18 35 87 10\n" +
                 "20 04 82 47 65\n" +
                 "19 01 23 75 03 34\n" +
                 "88 02 77 73 07 63 67\n" +
                 "99 65 04 28 06 16 70 92\n" +
                 "41 41 26 56 83 40 80 70 33\n" +
                 "41 48 72 33 47 32 37 16 94 29\n" +
                 "53 71 44 65 25 43 91 52 97 51 14\n" +
                 "70 11 33 28 77 73 17 78 39 68 17 57\n" +
                 "91 71 52 38 17 14 91 43 58 50 27 29 48\n" +
                 "63 66 04 68 89 53 67 30 73 16 69 87 40 31\n"+
                 "04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
                                                  //> strInput  : String = 75
                                                  //| 95 64
                                                  //| 17 47 82
                                                  //| 18 35 87 10
                                                  //| 20 04 82 47 65
                                                  //| 19 01 23 75 03 34
                                                  //| 88 02 77 73 07 63 67
                                                  //| 99 65 04 28 06 16 70 92
                                                  //| 41 41 26 56 83 40 80 70 33
                                                  //| 41 48 72 33 47 32 37 16 94 29
                                                  //| 53 71 44 65 25 43 91 52 97 51 14
                                                  //| 70 11 33 28 77 73 17 78 39 68 17 57
                                                  //| 91 71 52 38 17 14 91 43 58 50 27 29 48
                                                  //| 63 66 04 68 89 53 67 30 73 16 69 87 40 31
                                                  //| 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
	val dim2 = strInput.split("\n").map(x => x.split(" ").map(str => str.toInt))
                                                  //> dim2  : Array[Array[Int]] = Array(Array(75), Array(95, 64), Array(17, 47, 8
                                                  //| 2), Array(18, 35, 87, 10), Array(20, 4, 82, 47, 65), Array(19, 1, 23, 75, 3
                                                  //| , 34), Array(88, 2, 77, 73, 7, 63, 67), Array(99, 65, 4, 28, 6, 16, 70, 92)
                                                  //| , Array(41, 41, 26, 56, 83, 40, 80, 70, 33), Array(41, 48, 72, 33, 47, 32, 
                                                  //| 37, 16, 94, 29), Array(53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14), Array(7
                                                  //| 0, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57), Array(91, 71, 52, 38, 17, 1
                                                  //| 4, 91, 43, 58, 50, 27, 29, 48), Array(63, 66, 4, 68, 89, 53, 67, 30, 73, 16
                                                  //| , 69, 87, 40, 31), Array(4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 
                                                  //| 4, 23))
	
	def getMaxSum(dim2Arr: Array[Array[Int]]): Int = {
	  def calcMaxSum(curArr: Array[Int], prevArr: Seq[Int]): Seq[Int] =
	    for (i <- (0 until curArr.length))
	    yield (curArr(i) + (prevArr(i) max prevArr(i+1)))
	  
	  def process(row: Int, prevArr: Seq[Int]): Int = {
	    val nextArr: Seq[Int] = calcMaxSum(dim2Arr(row), prevArr)
	    
	    if (row==0) nextArr(0)
	    else process(row-1, nextArr)
	  }
	  
	  process(dim2Arr.length-2, dim2Arr(dim2Arr.length-1))
	}                                         //> getMaxSum: (dim2Arr: Array[Array[Int]])Int
	
	getMaxSum(dim2)                           //> res4: Int = 1074
	
	// prob019
	def isLeapYear(year: Int): Boolean =
	  if (year%400 == 0) true
	  else if (year%100 == 0) false
	  else if (year%4 == 0) true
	  else false                              //> isLeapYear: (year: Int)Boolean
	  
	def countDay(year: Int, month: Int): Int = month match {
	  case 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
	  case 4 | 6 | 9 | 11 => 30
	  case 2 if (isLeapYear(year)) => 29
	  case 2 => 28
	}                                         //> countDay: (year: Int, month: Int)Int
	  
	365%7                                     //> res5: Int(1) = 1
	// 1901.01.01 = Tuesday
	
	def nextMonthWeekday(year: Int, curMonth: Int, curWeekday: Int): Int = {
	  (curWeekday+countDay(year, curMonth))%7
	}                                         //> nextMonthWeekday: (year: Int, curMonth: Int, curWeekday: Int)Int
	
	def cntSunday(sYear: Int, sMonth: Int, sWeekday: Int, eYear: Int, eMonth: Int, acc: Int): Int = {
	  if (sYear==eYear && sMonth==eMonth) {
	    if (sWeekday==0) acc+1 else acc
	  } else {
	    val nextYear = if (sMonth==12) sYear+1 else sYear
	    val nextMonth = if (sMonth==12) 1 else sMonth+1
	    
	    cntSunday(nextYear, nextMonth, nextMonthWeekday(sYear, sMonth, sWeekday), eYear, eMonth, if (sWeekday==0) acc+1 else acc)
	  }
	}                                         //> cntSunday: (sYear: Int, sMonth: Int, sWeekday: Int, eYear: Int, eMonth: Int
                                                  //| , acc: Int)Int
	
	cntSunday(1901, 1, 2, 2000, 12, 0)        //> res6: Int = 171
	  
	
	// prob020
	factorial(100).toString.toCharArray.map((c:Char) => c.toString.toInt).sum
                                                  //> res7: Int = 648
	
	

                
}