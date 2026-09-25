/*
 * Copyright 2026 Polyvariant
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package jev4s.internal

import hearth.*
import hearth.kindlings.derivation.compiletime.AnnotationSupport
import hearth.std.StdExtensions
import jev4s.Options
import jev4s.description
import jev4s.label

/** Derives `Options[A]` for an enum or sealed trait of singleton cases, reading `@label` and
  * `@description`.
  */
private[jev4s] trait OptionsMacrosImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

  def deriveOptions[A: Type]: Expr[Options[A]] = {
    implicit val LabelT: Type[label] = Type.of[label]
    implicit val DescriptionT: Type[description] = Type.of[description]
    implicit val StringT: Type[String] = Type.of[String]
    implicit val OptionStringT: Type[Option[String]] = Type.of[Option[String]]

    val children = Type[A].directChildren.filter(_.nonEmpty).getOrElse {
      Environment.reportErrorAndAbort(
        s"${Type[A].prettyPrint} is not an enum or sealed trait with at least one case"
      )
    }

    val cases: List[Expr[(A, String, Option[String])]] = children.toList.map { (name, child) =>
      import child.Underlying as Case
      val value = Expr.singletonOf[Case].getOrElse {
        Environment.reportErrorAndAbort(
          s"$name in ${Type[A].prettyPrint} has fields; only parameterless cases can be options"
        )
      }
      val caseLabel = Expr(getTypeAnnotationStringArg[label, Case].getOrElse(name))
      val caseDescription = Expr(getTypeAnnotationStringArg[description, Case])
      Expr.quote {
        (Expr.splice(value.upcast[A]), Expr.splice(caseLabel), Expr.splice(caseDescription))
      }
    }

    Expr.quote(Options.fromCases[A](Expr.splice(VarArgs.from(cases))*))
  }

}
