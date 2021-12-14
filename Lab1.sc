import scala.math.BigInt
import BigInt.probablePrime
import scala.util.Random
import scala.language.postfixOps

//Zadanie 1
println("Zadanie 1:")
var temp1 = 1
temp1+2
val temp2 = 2
temp2+2
//Zadanie 2
println("Zadanie 2:")
"crazy"*3
//Zadanie 3
println("Zadanie 3:")
10 max 2
//Zadanie 4
println("Zadanie 4:")
BigInt(2) pow 1024
//Zadanie 5
println("Zadanie 5:")
probablePrime(100, Random)
//Zadanie 6
println("Zadanie 6:")
probablePrime(100, Random).toString(36)
//Zadanie 7
println("Zadanie 7:")
val s = "String"
s.head
s(0)
s.last
s(s.length - 1)
//Zadanie 8
println("Zadanie 8:")
// take: Selects the first n elements
s.take(2)
// drop: Selects all elements except first n ones
s.drop(2)
// takeRight: Selects the last n elements
s.takeRight(2)
// dropRight: Selects all elements except last n ones
s.dropRight(2)
//Zadanie 9
println("Zadanie 9:")
def Task9(n:Int): Int={
  var s = 0
  if (n > 0) s = 1
  else if (n == 0) s = 0
  else s = -1
  s
}
println(Task9(8))
println(Task9(0))
println(Task9(-4221))
//Zadanie 10
println("Zadanie 10:")
val empty= {}
//Zadanie 11
println("Zadanie 11:")
for(i<- 10 to 0 by -1) println(i)
//Zadanie 12
println("Zadanie 12:")
def countdown(n:Int) = for(i <- n to 0 by -1) println(i)
countdown(10)
//Zadanie 13
println("Zadanie 13:")
(for(i <- "Hello") yield i.toLong).product
//Zadanie 14
println("Zadanie 14:")
def product(s:String) = s.map(c => c.toLong).product
println(product("Hello"))
//Мар представляет данные в виде структуры, продукт перемножение
//Zadanie 16
println("Zadanie 15:")
def product(s:String, p:Long):Long = s.isEmpty match {
  case false => product(s.tail, p * s.head.toLong)
  case true => p
}

def prod(s:String):Long = s.isEmpty match {
  case false => product(s, 1)
  case true => 0
}
println(prod(""))
println(prod("Hello"))
//Zadanie 17
println("Zadanie 16:")
def Task16(x: Double, n: Int): Double = {
  if(n==0) 1
  else if (n>0 && n%2 == 0) Task16(x,n/2) * Task16(x,n/2)
  else if (n>0 && n%2 == 1) x * Task16(x,n-1)
  else 1 / Task16(x, -n)
}
println(Task16(2,7))
//Zadanie 18
println("Zadanie 17:")
def dub(number:Int): Int ={
  var num = number
  var s:Int = 0
  while(num>=10){
    if(num%10==((num/10)%10)){
      s=s+1
    }
    num=num/10
  }
  return s
}
def sum(m:Int, n:Int): Unit ={
  var summa:Int=0
  for(i<-m to n){
    if (dub(i)==0){
      summa=summa+i
    }
  }
  println(summa)
}
sum(10,20)
///
def listPrinter(n: List[Any]){
  for (i <- n){
    print(i + ", ")
  }
}
//Zadanie 19
println("Zadanie 18:")
var Task18_ans: List[Any] = List()
def Task18(n: List[Any]): List[Any]={
  for (i <- n){
    if (i.isInstanceOf[Int]) Task18_ans = Task18_ans :+ i
    else (Task18(i.asInstanceOf[List[Any]]))
  }
  Task18_ans
}

def Task18Printer(n: List[Any]){
  var printable = Task18(n)
  println(printable)
}
Task18Printer(List(List(1, 1), 2, List(3, List(5, 8))))


//Zadanie 20
println("Zadanie 19:")
def compose(n: Int): Boolean = {
  var t_temp19:Boolean=true
  for (i <- 2 to n-1) {
    if(n%i==0){
      t_temp19= false
    }
  }
  return t_temp19
}
println(compose(3))
def divider(n:Int): Int ={
  var temp19 = 0
  for (i<-1 to n){
    if(n%i==0&&compose(i)==true){
      temp19=i
    }
  }
  return temp19
}
println(divider(15))
def Task19(n:Int): Unit ={
  var sub_sum19 = 0
  var sub_n=n
  while(sub_n>10){
    sub_sum19=sub_sum19+(sub_n%10)
    sub_n=sub_n/10
  }
  println(sub_sum19+sub_n)
}
Task19(divider(15))

//Zadanie 21
println("Zadanie 20:")
def Task20(n: List[Any], k: Int){
  var ans: List[Any] = List()
  for (i <- n){
    for (reps <- 1 to k){
      ans = ans :+ i
    }
  }
  println(ans)
}
Task20(List(1,2,5,7,9),3)
//Zadanie 24
println("Zadanie 21:")
def gcd(m: Long, n: Long): Long ={//наибольший общий делитель, которое делится на m и n без остатка.
  if (n == 0) m
  else gcd(n, m % n)
}
def Task21(m: Long, n: Long): Long ={//наименшее общее кратное по формуле
  m / gcd(m,n) * n
}
println(Task21(35,21))
//Zadanie 25
println("Zadanie 22:")
def Task22(n: List[Any], delets: Int){
  var ans: List[Any] = List()
  var counter = 0
  for (i <- n){
    counter += 1
    if (counter < delets) ans = ans :+ i
    else counter = 0

  }
  println(ans)
}
Task22(List(1,3,4,5,2,3), 3)
//Zadanie 26
println("Zadanie 23:")
def fact(n:Int): Int ={
  var ans = 1
  for (i <- 1 to n){
    ans *= i
  }
  ans
}
def Task23(n: Int, k: Int): Int ={
  if (k > n) -1
  else {
    fact(n) / fact(n-k)
  }
}
Task23(4,2)
//Zadanie 27
println("Zadanie 24:")
def Task24(arr: List[Any], n: Int): Unit = {
  //var arr:List[Any] = List(1,2,3,4,5,6,7,8,9,0)
  //var n: Int = readLine("Get a number: ").toInt
  if(n>0) {
    def left(l: List[Any]): List[Any] = {
      l.takeRight(l.length - n) ++ l.take(n)
    }
    println(left(arr))
  }else {
    def right(r: List[Any]): List[Any] = {
      r.takeRight(-n) ++ r.take(r.length + n)
    }

    println(right(arr))
  }
}
Task24(List(1,2,3,4,5,6,7,8,9,0), 3)
//Zadanie 28
println("Zadanie 25:")
def perfect(n: Int): Boolean ={//проверка на совершенность
  var summa = 0
  for (i <- 1 until n){
    if(n % i == 0) summa += i
  }
  summa == n
}
def Task25(n: Int): Int ={//провчеряем все числа от 1 до н
  for (i <- 1 to n reverse){
    if (perfect(i)) return i
  }
  1
}
println(Task25(7))
//Zadanie 29
println("Zadanie 26:")
def Task26(n: List[Any]){
  val indexlength = n.length - 1

  var even: List[Any] = List()
  var odd: List[Any] = List()

  for (i <- 0 to indexlength){
    if (i % 2 == 0) even = even :+ n(i)
    else odd = odd :+ n(i)
  }
  print("Chet:")
  listPrinter(even)
  print("Nechet:")
  listPrinter(odd)
}
Task26(List(0,1,2,3,4,5,6,7,8,9))
//Zadanie 30
println("Zadanie 27:")
def sumDigits(n: Int): Int ={//получаем сумму цифр числа
  var result = 0
  var num = n
  while (num > 0){
    result += num % 10
    num /= 10
  }
  result
}
def isPow(n: Int): Boolean ={//умножаем сумму цифр на себя до получения исходного числа
  var result = 1
  while (result < n && sumDigits(n) != 1){
    result *= sumDigits(n)
  }
  result == n
}
def Task27(n: Int): Int ={
  for (i <- 1 until n reverse){
    if (isPow(i)) return i
  }
  -1
}
println(Task27(81))

//Zadanie 31
println("Zadanie 28:")
def Task28(nn: List[Any]){
  val indexlength = nn.length - 1

  var n, s: List[Any] = List()

  for (i <- 0 to indexlength){
    n = n :+ nn(i).asInstanceOf[List[Any]](0)
    s = s :+ nn(i).asInstanceOf[List[Any]](1)
  }
  listPrinter(n)
  listPrinter(s)

}
Task28(List(List(1,"one"), List(2,"two"), List(3,"three")))