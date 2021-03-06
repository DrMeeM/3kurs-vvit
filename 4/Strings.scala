object Strings {

  /* a) Преобразуйте все символы типа Char в верхний регистр (не используйте заглавные буквы). */
  def testUppercase(str: String): String = str.toUpperCase()

  /* b) Вставьте следующие значения в строку:Hi my name is <name> and I am <age> years old.*/
  def testInterpolations(name: String, age: Int): String = s"Hi my name is $name and I am $age years old"

  /* c) Добавьте два числа в следующую строку:
   *       Hi,
   *       now follows a quite hard calculation. We try to add:
   *         a := <value of a>
   *         b := <value of b>
   *         result is <a + b>*/
  def testComputation(a: Int, b: Int): String = s"Hi, \nnow follows a quite hard calculation. We try to add:\na := $a\nb := $b\nresult is ${a + b}"

  /* d) Если длина строки равна 2, верните всю строку, иначе верните первые два символа строки.*/
  def testTwoChar(str: String): String = {
    if(str.length==2) str else str.substring(0,2)
  }

  def main(args:Array[String])={
    print("Task1:  ")
    println(testUppercase("sample text"))
    print("Task2:  ")
    println(testInterpolations("Alexander",19))
    println("Task3:")
    println(testComputation(12,5))
    print("Task4: \n")
    println(testTwoChar("Sample Text"))
    println(testTwoChar("Pr"))
  }
}
