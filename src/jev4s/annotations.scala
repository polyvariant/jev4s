package jev4s

import scala.annotation.StaticAnnotation

/** Overrides the option key sent to the model for an enum case. Defaults to the case name. */
final class label(val value: String) extends StaticAnnotation

/** Describes what an enum case means: a Choice option's rubric, or a Score level's description. */
final class description(val value: String) extends StaticAnnotation
