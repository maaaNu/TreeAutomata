package de.university_stuttgart.maaanu.parsers

object HeightParser extends AbstractParser {

  type T = Int
  def func_term(r:String, t: String): Int = 0
  def func_expr(r: String, e: Int): Int = e + 1
  def func_expr_wrapper(r: String, e: List[Int]): Int = 1 + max(e)
  def func_head(e: Int): Int = e + 1

  def max(xs: List[Int]): Int = xs match {
    case List(x: Int) => x
    case x :: y :: rest => max((if (x > y) x else y) :: rest)
    case Nil => 0
  }

}