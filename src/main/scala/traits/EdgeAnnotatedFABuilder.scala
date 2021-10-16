
package org.maraist.fa.traits
import scala.collection.mutable.Builder
import org.maraist.fa.elements
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle

/** Methods for the builder of any finite automaton.
  *
  * @tparam S Type representing states.
  * @tparam T Type representing transition labels.
  * @tparam A Type of automaton constructed.
  * @tparam Z Type of style options for Graphviz export.
  */
trait EdgeAnnotatedFABuilder[
  S, T, A,
  +M[DS, DT, DA] <: EdgeAnnotatedFA[DS, DT, DA, Z],
  -K >: elements.EdgeAnnotatedFAelements[S, T, A] <: Matchable,
  -Z[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA]
]

extends FABuilder[
  S, T,
  [MS, MT] =>> M[MS, MT, A],
  K,
  [ZS, ZT] =>> Z[ZS, ZT, A]]

with UnindexedEdgeAnnotatedFA[S, T, A, Z] {

  /** Set the annotation on the transition from `src` to `dest` labelled
    * `label`.
    */
  def setAnnotation(src: S, label: T, dest: S, annotation: A): Unit

  /** Remove any annotation from the transition from `src` to `dest`
    * labelled `label`.
    */
  def removeAnnotation(src: S, label: T, dest: S): Unit
}
