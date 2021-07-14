// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.annotated
import scala.collection.mutable.{HashMap,HashSet,Queue}
import org.maraist.graphviz.{Graphable,NodeLabeling,TransitionLabeling}
import org.maraist.fa.elements.HasBuilder
import org.maraist.fa.{DFA, NDFA, DFABuilder, NDFABuilder}
import org.maraist.fa.DFA.{IndexedDFA, DFAelements}
import org.maraist.fa.NDFA.{IndexedNDFA, NDFAelements}

/** Operations for converting NDFA to DFA annotations, and for
  * combining pairs of annotations, when converting an annotated NDFA
  * into an annotated DFA.
  *
  * @tparam A The type of annotations on transitions in the NDFA
  * @tparam K Type function/contructor producing the type of annotation
  *
  * @group Annotated
  */
trait EdgeAnnotationCombiner[A, K[_]] {

  /** Lift a single annotation to the result annotation type. */
  def single(a: A): K[A]

  /** Incorporate another annotation. */
  def include(k: K[A], a: A): K[A]

  protected[fa] def updated(prev: Option[K[A]], ann: A): K[A] = prev match {
    case None => single(ann)
    case Some(prevAnn) => include(prevAnn, ann)
  }

  /** Combine two converted annotations. */
  def combine(k1: K[A], k2: K[A]): K[A]

  protected[fa] def combined(prev: Option[K[A]], curr: K[A]): K[A] =
    prev match {
      case None => curr
      case Some(prev) => combine(prev, curr)
    }
}

object Elements {
  case class SetAnnotation[S,T,A](src: S, dest: S, label: T, annotation: A)
  case class RemoveAnnotation[S,T,A](src: S, dest: S, label: T)

  type LabelledEdgeAnnotationElements[S,T,A] =
    SetAnnotation[S,T,A] | RemoveAnnotation[S,T,A]

  type AnnotatedDFAelement[S,T,A] =
    DFAelements[S,T] | LabelledEdgeAnnotationElements[S,T,A]

  case class SetEAnnotation[S,A](src: S, dest: S, annotation: A)
  case class RemoveEAnnotation[S,A](src: S, dest: S)

  type UnlabelledEdgeAnnotationElements[S,A] =
    SetEAnnotation[S,A] | RemoveEAnnotation[S,A]

  type AnnotatedNDFAelement[S,T,A] = (
    NDFAelements[S,T] | LabelledEdgeAnnotationElements[S,T,A]
      | UnlabelledEdgeAnnotationElements[S,A]
  )
}

/** Methods provided by an edge-annotated nondeterministic finite
  * automata (NDFA).
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam A The type of annotations on transitions
  * @tparam K Type function/contructor producing the type of
  * annotation on DFAs from the type of annotation on NDFAs.
  * @tparam D Type of DFA converted from this NDFA
  *
  * @group Annotated
  */
trait EdgeAnnotatedNDFA
  [S, T, A, K[_], +D <: IndexedDFA[Set[S],T] & EdgeAnnotatedDFA[Set[S],T,K[A]]]
  (using combo: EdgeAnnotationCombiner[A,K])
    extends NDFA[S, T, D] {

  /** Return the annotation (if any) on the transition from `src` to
    * `dest` labelled `label`.
    */
  def annotation(src: S, label: T, dest: S): Option[A]

  /** Check whether there is an annotation on the transition from `src`
    * to `dest` labelled `label`.
    */
  def annotated(src: S, dest: S, label: T): Boolean =
    annotation(src, label, dest).isDefined

  /** Return the annotation (if any) on the e-transition from `src` to
    * `dest`.
    */
  def annotation(src: S, dest: S): Option[A]

  /** Check whether there is an annotation on the unlabeled transition
    * from `src` to `dest`.
    */
  def annotated(src: S, dest: S): Boolean =
    annotation(src, dest).isDefined
}

object EdgeAnnotatedNDFA {

  def newBuilder[S, T, A, K[_]](using combo: EdgeAnnotationCombiner[A,K]):
      EdgeAnnotatedNDFA
        [S, T, A, K,
          ? <: IndexedDFA[Set[S],T] & EdgeAnnotatedDFA[Set[S],T,K[A]]] =
    new HashEdgeAnnotatedNDFABuilder[S, T, K, A]

}

/** Methods to be implemented by an edge-annotated NDFA builder.
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the
  * automaton
  * @tparam A The type of annotations on transitions
  * @tparam K Type function/contructor producing the type of
  * annotation on DFAs from the type of annotation on NDFAs.
  * @tparam D Type of DFA converted from this NDFA
  *
  * @group Annotated
  */
trait NDFAEdgeAnnotationsBuilder
  [S, T, A, K[_],
    +D <: IndexedDFA[Set[S],T] & EdgeAnnotatedDFA[Set[S],T,K[A]],
    +N <: EdgeAnnotatedNDFA[S,T,A,K,D],
    E >: Elements.AnnotatedNDFAelement[S,T,A] <: Matchable
  ]
    extends EdgeAnnotatedNDFA[S, T, A, K, D] {
  this: NDFABuilder[S, T, D, N, E] =>

  /** Set the annotation on the transition from `src` to `dest` labelled
    * `label`.
    */
  def setAnnotation(src: S, dest: S, label: T, annotation: A): Unit

  /** Remove any annotation from the transition from `src` to `dest`
    * labelled `label`.
    */
  def removeAnnotation(src: S, dest: S, label: T): Unit

  /** Set the annotation on the unlabelled transition from `src` to
    * `dest`.
    */
  def setEAnnotation(src: S, dest: S, annotation: A): Unit

  /** Remove any annotation from the unlabelled transition from `src` to
    * `dest`.
    */
  def removeEAnnotation(src: S, dest: S): Unit
}

/** Methods to be implemented by an edge-annotated DFA.
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the
  * automaton
  * @tparam A The type of annotations on transitions
  *
  * @group Annotated
  */
trait EdgeAnnotatedDFA[S,T,A] extends DFA[S, T] {

  /** Return the annotation (if any) on the transition from `src` to
    * `dest` labelled `label`.
    */
  def annotation(src: S, label: T): Option[A]
}

/** Methods to be implemented by an edge-annotated DFA builder.
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the
  * automaton
  * @tparam A The type of annotations on transitions
  * @tparam D Type of DFA returned from this builder.  Should
  * implement [[EdgeAnnotatedDFA]].
  *
  * @group Annotated
  */
trait DFAEdgeAnnotationsBuilder[
  S, T, K[_], A, +D <: EdgeAnnotatedDFA[S,T,A],
  E >: Elements.AnnotatedDFAelement[S,T,A] <: Matchable]
    extends EdgeAnnotatedDFA[S, T, A] {
  this: DFABuilder[S, T, D, E] =>

  /** Set the annotation on the transition from `src` to `dest` labelled
    * `label`.
    */
  def setAnnotation(src: S, label: T, annotation: A): Unit

  /** Remove any annotation from the transition from `src` to `dest`
    * labelled `label`.
    */
  def removeAnnotation(src: S, label: T): Unit
}
