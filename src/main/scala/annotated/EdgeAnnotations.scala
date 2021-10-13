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
import org.maraist.graphviz.{Graphable}
import org.maraist.fa.elements.HasBuilder
import org.maraist.fa.{DFA, NFA, DFABuilder, NFABuilder}
import org.maraist.fa.DFA.{IndexedDFA, DFAelements}
import org.maraist.fa.NFA.{IndexedNFA, NFAelements}

object EdgeAnnotatedNFA {

  def newBuilder[S, T, NA, DA](using combo: EdgeAnnotationCombiner[NA, DA]):
      NFAEdgeAnnotationsBuilder[S, T, NA, DA,
        ? <: IndexedDFA[Set[S],T] & EdgeAnnotatedDFA[Set[S],T,DA],
        ? <: EdgeAnnotatedNFA[S,T,NA,DA,?],
        Elements.AnnotatedNFAelement[S,T,NA]] =
    new HashEdgeAnnotatedNFABuilder[S, T, DA, NA]

}

/** Methods to be implemented by an edge-annotated NFA builder.
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the
  * automaton
  * @tparam NA The type of annotations on transitions in the NFA.
  * @tparam DA The type of annotations on transitions in the DFA.
  * @tparam D Type of DFA converted from this NFA
  *
  * @group Annotated
  */
trait NFAEdgeAnnotationsBuilder
  [S, T, NA, DA,
    +D <: IndexedDFA[Set[S],T] & EdgeAnnotatedDFA[Set[S],T,DA],
    +N <: EdgeAnnotatedNFA[S,T,NA,DA,D],
    E >: Elements.AnnotatedNFAelement[S,T,NA] <: Matchable
  ]
    extends NFABuilder[S, T, D, N, E]
    with EdgeAnnotatedNFA[S, T, NA, DA, D] {

  /** Set the annotation on the transition from `src` to `dest` labelled
    * `label`.
    */
  def setAnnotation(src: S, label: T, dest: S, annotation: NA): Unit

  /** Remove any annotation from the transition from `src` to `dest`
    * labelled `label`.
    */
  def removeAnnotation(src: S, label: T, dest: S): Unit

  /** Set the annotation on the unlabelled transition from `src` to
    * `dest`.
    */
  def setEAnnotation(src: S, dest: S, annotation: NA): Unit

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

  override protected def dumpTransition(src: S, label: T, dest: S): Unit = {
    print("- " + src + " -[ " + label)
    annotation(src, label) match {
      case None => { print(" (unann.)") }
      case Some(a) => { print(" : " + a) }
    }
    print(" ]-> " + dest)
    println()
  }
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
  S, T, A, +D <: EdgeAnnotatedDFA[S,T,A],
  E >: Elements.AnnotatedDFAelement[S,T,A] <: Matchable]
    extends DFABuilder[S, T, D, E] with EdgeAnnotatedDFA[S, T, A] {

  /** Set the annotation on the transition from `src` to `dest` labelled
    * `label`.
    */
  def setAnnotation(src: S, label: T, annotation: A): Unit

  /** Remove any annotation from the transition from `src` to `dest`
    * labelled `label`.
    */
  def removeAnnotation(src: S, label: T): Unit
}
