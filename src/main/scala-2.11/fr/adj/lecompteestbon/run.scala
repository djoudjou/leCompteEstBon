package fr.adj.lecompteestbon

/**
 * Created by djoutsop on 16/09/2015.
 */
object run {

  def format(expr: Expr): String =
    expr match {
      case Number(value) => value.toString
      case Add(left, right) => format(left) + " + " + format(right)
      case Sub(left, right) => format(left) + " - " + format(right)
      case Mul(left, right) => format(left) + " * " + format(right)
      case Div(left, right) => format(left) + " / " + format(right)
    }

  def eval(expr: Expr): Int =
    expr match {
      case Number(value) => value
      case Add(left, right) => eval(left) + eval(right)
      case Sub(left, right) => eval(left) - eval(right)
      case Mul(left, right) => eval(left) * eval(right)
      case Div(left, right) => eval(left) / eval(right)
    }

  def print(expr: Expr) = {
    println(format(expr))
    println(eval(expr))
  }

  def removeEltAtPositions(elements: List[Int], positions: Int*) = {
    for {
      (x, idx) <- elements.zipWithIndex
      if !positions.contains(idx)
    } yield x
  }

  def main(args: Array[String]) {

    val expr1 = Mul(Number(10), Number(100))
    val expr2 = Div(Number(20), Number(5))
    val expr = Add(expr1, expr2)

    //100 - 4 2 50
    //val prob = Problem(guessNumber = 100, 4, 2, 50)
    //print(prob.next())

    val operators = List("*", "/", "+", "-")
    val list = List(1, 2, 3, 4)

    for{
      combinaison <- list.permutations.toList
      number <-combinaison
      op <- operators
    } yield println(s"$number $op")

    //println(list.permutations.mkString("\n"))

  }
}
