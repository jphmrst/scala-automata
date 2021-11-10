
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
trait EdgeAnnotatedDFABuilder[
  S, T, A,
  +D[DS, DT, DA] <: EdgeAnnotatedDFA[DS, DT, DA, Z],
  -K >: elements.EdgeAnnotatedDFAelements[S, T, A] <: Matchable,
  -Z[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA]
]

extends EdgeAnnotatedFABuilder[S, T, A, D, K, Z]

with UnindexedEdgeAnnotatedDFA[S, T, A, Z] {

  /** Set the annotation on the transition from `src` labelled `label`.
    */
  def setAnnotation(src: S, label: T, annotation: A): Unit

  /** Remove any annotation from the transition from `src` labelled
    * `label`.
    */
  def removeAnnotation(src: S, label: T): Unit

  /** Set the initial annotation "before" the initial state.
    */
  def addInitialAnnotation(annotation: A): Unit

  /** Remove any initial annotation "before" the initial state.
    */
  def removeInitialAnnotation: Unit
}
