package jev4s.internal

import hearth.MacroCommonsScala3
import hearth.kindlings.derivation.compiletime.AnnotationSupport
import jev4s.Options

import scala.quoted.*

final private[jev4s] class OptionsMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupport,
      OptionsMacrosImpl

private[jev4s] object OptionsMacros {
  def derive[A: Type](using q: Quotes): Expr[Options[A]] = new OptionsMacros(q).deriveOptions[A]
}
