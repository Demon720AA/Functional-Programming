import scala.io.StdIn.*

def successor(num: Int): Int =
  //ค่าถัดไปของตัวเลข
  num + 1

def predecessor(num: Int): Int =
  //ค่าก่อนหน้าของตัวเลข
  num - 1

def not(num: Double): Double =
  //NOT
  -num

def addition(num1: Int, num2: Int): Int =
  //การบวก
  if (isZero(num2)) then
    num1
  else
    addition(successor(num1), predecessor(num2))

def subtraction(num1: Int, num2: Int): Int =
  //การลบ
  if (isZero(num2)) then
    num1
  else
    addition(predecessor(num1), successor(num2))

def multiplication(num1: Int, num2: Int): Int =
  //การคูณ
  if !isZero(num2) then
    num1 + multiplication(num1, predecessor(num2))
  else
    num2
def and(num1: Boolean, num2: Boolean): Boolean =
  //AND
  if (num1) then
    num2
  else
    false

def or(num1: Boolean, num2: Boolean): Boolean =
  //OR
  if (num1) then
    true
  else
    num2

def isEven(num: Int): Boolean =
  //เช็คเลขคู่
  if (isZero(num)) then
    true
  else
    isOdd(predecessor(num))

def isOdd(num: Int): Boolean =
  //เช็คเลขคี่
  if (isZero(num)) then
    false
  else
    isEven(predecessor(num))

def square(num: Int): Int =
  //ยกกำลัง2
  multiplication(num, num)

def isZero(num: Int): Boolean =
  //เช็คศูนย์
  num == 0

def exponentiation(num1: Int, num2: Int): Int =
  //ยกกำลัง
  if (isZero(num2)) then
    1
  else
    multiplication(num1, exponentiation(num1, predecessor(num2)))

def leq(num1: Int, num2: Int): Boolean =
  //ตรวจสอบ <=
  if (isZero(num1)) then
    true
  else if (isZero(num2)) then
    false
  else
    leq(predecessor(num1), predecessor(num2))

def equality(num1: Int, num2: Int): Boolean =
  and(leq(num1, num2), leq(num2, num1))


@main
def main(): Unit =
  print("Enter Num1 : ")
  val num1 = readInt()
  print("Enter Num2 : ")
  val num2 = readInt()
  print("Enter Boolean1 : ")
  val boo1 = readBoolean()
  print("Enter Boolean2 : ")
  val boo2 = readBoolean()
  println("Num1 Successor : " + successor(num1))
  println("Num2 Successor : " + successor(num2))
  println("Num1 Predecessor : " + predecessor(num1))
  println("Num2 Predecessor : " + predecessor(num2))
  println("Not Num1 : " + not(num1))
  println("Addition : " + addition(num1, num2))
  println("Subtraction : " + subtraction(num1, num2))
  println("Multiplication : " + multiplication(num1, num2))
  println("And : " + and(boo1, boo2))
  println("Or : " + or(boo1, boo2))
  println("Num1 IsEven : " + isEven(num1))
  println("Num2 IsEven : " + isEven(num2))
  println("Num1 IsOdd : " + isOdd(num1))
  println("Num2 IsOdd : " + isOdd(num2))
  println("Num1 Square : " + square(num1))
  println("Num2 Square : " + square(num2))
  println("Num1 IsZero : " + isZero(num1))
  println("Num2 IsZero : " + isZero(num2))
  println("Exponentiation : " + exponentiation(num1, num2))
  println("Less Than or Equal to : " + leq(num1, num2))
  println("Equality :" + equality(num1, num2))