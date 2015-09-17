package fr.adj.lecompteestbon

import scala.util.Random

trait Expr


case class Number(num: Int) extends Expr

case class Mul(left: Expr, right: Expr) extends Expr

case class Div(left: Expr, right: Expr) extends Expr

case class Add(left: Expr, right: Expr) extends Expr

case class Sub(left: Expr, right: Expr) extends Expr



case class Problem(guessNumber: Int, numbers: Int*) {

  private val random = new Random

  val _guessNumber = Number(guessNumber)


  val operators = List("*", "/", "+", "-")

  def eval(expr: Expr): Int =
    expr match {
      case Number(value) => value
      case Add(left, right) => eval(left) + eval(right)
      case Sub(left, right) => eval(left) - eval(right)
      case Mul(left, right) => eval(left) * eval(right)
      case Div(left, right) => eval(left) / eval(right)
    }

  def getRandomNumber(numbers: List[Number]): (Number, List[Number]) = {
    val pos = random.nextInt(numbers.length)

    val remainNumbers = for {
      (x, i) <- numbers.zipWithIndex
      if i != pos
    } yield x

    (numbers(pos), remainNumbers)
  }


  def doPickNumber(remainingNumbers: List[Number]): Boolean = {
    //!remainingNumbers.isEmpty && random.nextInt(remainingNumbers.length + operators.length) < remainingNumbers.length
    !remainingNumbers.isEmpty && random.nextInt(2)==1
  }





  def recGenerator(remainingNumbers: List[Number]): (Expr,List[Number]) = {


    if (!doPickNumber(remainingNumbers)) {

      val (left, remainingNumbers1) = recGenerator(remainingNumbers)
      val (right, remainingNumbers2) = recGenerator(remainingNumbers1)

      Random.nextInt(4) match {
        case 0 => (Mul(left, right),remainingNumbers2)
        case 1 => (Div(left, right),remainingNumbers2)
        case 2 => (Add(left, right),remainingNumbers2)
        case 3 => (Sub(left, right),remainingNumbers2)
      }

    } else {
      getRandomNumber(remainingNumbers)
    }
  }

  def next(): Expr = {
    recGenerator(numbers.toList.map { Number(_) })._1
  }

}