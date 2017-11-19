package de.university_stuttgart.maaanu.parsers

object DeleteTraceParser extends AbstractParser {
  type T = String

  def func_term(r: String, t: String): String = t.charAt(0) match {
    case '*' => "*"
    case _ => "(" + r + " " + t + ")"
  }

  def func_expr(r: String, e: String): String = e match {
      case "*" => "*"
      case _ => "(" + r + " " + e + ")"
  }

  def func_expr_wrapper(r: String, e: List[String]): String = {
    if (check(e)) return "*"
    "(" + r + " " + e.filter { x => x.charAt(0) != '*' }.mkString(" ") + ")"
  }

  def func_head(e: T): T = "( " + e + " )"

  def check(xs: List[String]): Boolean = xs match {
    case List(x: String) => x.charAt(0) == '*'
    case x :: rest => x.charAt(0) == '*' && check(rest)
    case _ => false
  }

}