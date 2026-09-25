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

package jev4s

import scala.annotation.StaticAnnotation

/** Overrides the option key sent to the model for an enum case. Defaults to the case name. */
final class label(val value: String) extends StaticAnnotation

/** Describes what an enum case means: a Choice option's rubric, or a Score level's description. */
final class description(val value: String) extends StaticAnnotation
