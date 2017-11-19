package de.university_stuttgart.maaanu.parsers

import scala.util.parsing.combinator.RegexParsers

trait AbstractParser extends RegexParsers  {
  type T
  def start: Parser[T] = """\(\s""".r ~> expr <~ """\)""".r ^^ { func_head }
  def expr: Parser[T] = rule_name ~ rep(expr) <~ closing_bracket ^^ { case r ~ e => func_expr_wrapper(r ,e) } | term
  def term: Parser[T] = rule_name ~ """[^\)]+""".r <~ """\)""".r ^^ { case r ~ t => func_term(r, t) }
  def rule_name: Parser[String] = """\(""".r ~> """[^\s]+""".r ^^ { func_rule_name }
  def closing_bracket: Parser[String] = """\)[\s]?""".r ^^ { func_closing_brackets }
  
  def func_term(r: String, t:String): T
  def func_expr(r: String, e:T): T
  def func_expr_wrapper(r: String, e: List[T]): T
  def func_head(e: T): T
  
  def func_rule_name(r:String): String = r.toString
  def func_closing_brackets(b: String): String = b.toString
  
  
}