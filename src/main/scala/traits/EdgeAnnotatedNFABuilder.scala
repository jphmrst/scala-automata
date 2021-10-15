
package org.maraist.fa.traits
import scala.collection.mutable.Builder
import org.maraist.fa.elements.EdgeAnnotatedNFAelements
import org.maraist.fa.styles.AutomatonStyle

/** Methods for the builder of any finite automaton.
  *
  * @tparam S Type representing states.
  * @tparam T Type representing transition labels.
  * @tparam N Type of automaton constructed.
  * @tparam Z Type of style options for Graphviz export.
  */
trait EdgeAnnotatedNFABuilder[
  S, T, NA, DA,
  G[X] <: Set[X],
  +D[DS, DT, A] <: EdgeAnnotatedDFA[DS, DT, A, Z],
  +N[NS, NT, NNA, NDA] <: EdgeAnnotatedNFA[NS, NT, NNA, NDA, G, D, Z],
  -K >: EdgeAnnotatedNFAelements[S, T, NA] <: Matchable,
  -Z[ZS, ZT] <: AutomatonStyle[ZS, ZT]
]

extends NFABuilder[
  S, T, G,
  [DS, DT] =>> D[DS, DT, DA],
  [NS, NT] =>> N[NS, NT, NA, DA],
  K, Z]

with EdgeAnnotatedFABuilder[
  S, T, NA, [NS, NT, NNA] =>> N[NS, NT, NNA, DA], K, Z]

with UnindexedEdgeAnnotatedNFA[
  S, T, NA, DA, G, D, Z] {

  /** Set the annotation on the unlabelled transition from `src` to
    * `dest`.
    */
  def setEAnnotation(src: S, dest: S, annotation: NA): Unit

  /** Remove any annotation from the unlabelled transition from `src` to
    * `dest`.
    */
  def removeEAnnotation(src: S, dest: S): Unit
}
