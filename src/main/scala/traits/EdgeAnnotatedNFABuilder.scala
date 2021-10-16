
package org.maraist.fa.traits
import scala.collection.mutable.Builder
import org.maraist.fa.elements.EdgeAnnotatedNFAelements
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle

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
  +D[DS, DT, A] <: EdgeAnnotatedDFA[DS, DT, A, DZ],
  +N[NS, NT, NNA, NDA] <: EdgeAnnotatedNFA[NS, NT, NNA, NDA, G, D, NZ, DZ],
  -K >: EdgeAnnotatedNFAelements[S, T, NA] <: Matchable,
  -NZ[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA],
  -DZ[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA]
]

extends NFABuilder[
  S, T, G,
  [DS, DT] =>> D[DS, DT, DA],
  [NS, NT] =>> N[NS, NT, NA, DA],
  K,
  [ZS, ZT] =>> NZ[ZS, ZT, NA],
  [ZS, ZT] =>> DZ[ZS, ZT, DA]]

with EdgeAnnotatedFABuilder[
  S, T, NA, [NS, NT, NNA] =>> N[NS, NT, NNA, DA], K, NZ]

with UnindexedEdgeAnnotatedNFA[
  S, T, NA, DA, G, D, NZ, DZ] {

  /** Set the annotation on the unlabelled transition from `src` to
    * `dest`.
    */
  def setEAnnotation(src: S, dest: S, annotation: NA): Unit

  /** Remove any annotation from the unlabelled transition from `src` to
    * `dest`.
    */
  def removeEAnnotation(src: S, dest: S): Unit
}
