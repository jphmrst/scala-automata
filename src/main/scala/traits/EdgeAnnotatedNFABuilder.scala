
package org.maraist.fa.traits
import scala.collection.mutable.Builder
import org.maraist.fa.elements
import org.maraist.fa.styles.AutomatonStyle

/** Methods for the builder of any finite automaton.
  *
  * @tparam S Type representing states.
  * @tparam T Type representing transition labels.
  * @tparam A Type of automaton constructed.
  * @tparam Z Type of style options for Graphviz export.
  */
trait EdgeAnnotatedNFABuilder[
  S, T, NA, DA,
  G[X] <: Set[X],
  +D[DS, DT, A] <: EdgeAnnotatedDFA[DS, DT, A, Z],
  +N[NS, NT, NNA, NDA] <: EdgeAnnotatedNFA[NS, NT, NNA, NDA, G, D, Z],
  -K >: elements.EdgeAnnotatedNFAelements[S, T, NA] <: Matchable,
  -Z[ZS, ZT] <: AutomatonStyle[ZS, ZT]
]

extends FABuilder[S, T, [NS, NT] =>> N[NS, NT, NA, DA], K, Z]

with UnindexedEdgeAnnotatedFA[S, T, NA, Z] {

  /** Set the annotation on the transition from `src` to `dest` labelled
    * `label`.
    */
  def setAnnotation(src: S, label: T, dest: S, annotation: NA): Unit

  /** Remove any annotation from the transition from `src` to `dest`
    * labelled `label`.
    */
  def removeAnnotation(src: S, label: T, dest: S): Unit

//  /** Set the annotation on the unlabelled transition from `src` to
//    * `dest`.
//    */
//  def setEAnnotation(src: S, dest: S, annotation: NA): Unit
//
//  /** Remove any annotation from the unlabelled transition from `src` to
//    * `dest`.
//    */
//  def removeEAnnotation(src: S, dest: S): Unit
}
