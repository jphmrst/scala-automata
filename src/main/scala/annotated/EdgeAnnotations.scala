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
import org.maraist.fa.general.Builders.HasBuilder
import org.maraist.fa.{DFA, NDFA}
import org.maraist.fa.NDFA.IndexedNDFA
import org.maraist.fa.DFA.IndexedDFA

/** Methods provided by nondeterministic finite automata (NDFAs)
  *
  * Trait specifying methods provided by all NDFAs, and providing
  * default implementations for derivations from the core methods.
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam A The type of annotations on transitions
  * @tparam K Type function/contructor producing the type of annota
  *
  * @group Annotated
  */
trait NDFAEdgeAnnotations
  [S, T, A, K[_], +D <: IndexedDFA[Set[S],T] & DFAEdgeAnnotations[S,T,K[A]]]
  (using combo: EdgeAnnotationCombiner[A,K]) {

  this: NDFA[S, T, D] =>

  /** Return the annotation (if any) on the transition from `src` to
    * `dest` labelled `label`.
    */
  def annotation(src: S, dest: S, label: T): Some[A]

  /** Check whether there is an annotation on the transition from `src`
    * to `dest` labelled `label`.
    */
  def annotated(src: S, dest: S, label: T): Boolean =
    annotation(src, dest, label).isDefined

  /** Return the annotation (if any) on the e-transition from `src` to
    * `dest`.
    */
  def annotation(src: S, dest: S): Some[A]

  /** Check whether there is an annotation on the unlabeled transition
    * from `src` to `dest`.
    */
  def annotated(src: S, dest: S): Boolean =
    annotation(src, dest).isDefined
}

/**
  *
  * @group Annotated
  */
trait NDFAEdgeAnnotationsBuilder
  [S, T, A, K[_], +D <: IndexedDFA[Set[S],T] & DFAEdgeAnnotations[S,T,K[A]]]
    extends NDFAEdgeAnnotations[S, T, A, K, D] {
  this: NDFA[S, T, D] =>

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

/**
  *
  * @group Annotated
  */
trait DFAEdgeAnnotations[S,T,A] {
  this: DFA[S, T] =>

  /** Return the annotation (if any) on the transition from `src` to
    * `dest` labelled `label`.
    */
  def annotation(src: S, dest: S, label: T): Some[A]
}

/**
  *
  * @group Annotated
  */
trait EdgeAnnotationCombiner[A, K[_]] {

  /** Lift a single annotation to the result annotation type. */
  def single(a: A): K[A]

  /** Combine in another annotation. */
  def combine(k: K[A], a: A): K[A]
}
