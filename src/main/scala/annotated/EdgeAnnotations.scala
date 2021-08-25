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
trait EdgeAnnotationCombiner[NA, DA] {

  /** Lift a single annotation to the result annotation type. */
  def single(a: NA): DA

  /** Incorporate another annotation. */
  def include(k: DA, a: NA): DA

  protected[fa] def updated(prev: Option[DA], ann: NA): DA = prev match {
    case None => single(ann)
    case Some(prevAnn) => include(prevAnn, ann)
  }

  /** Combine two converted annotations. */
  def combine(k1: DA, k2: DA): DA

  protected[fa] def combined(prev: Option[DA], curr: DA): DA =
    prev match {
      case None => curr
      case Some(prev) => combine(prev, curr)
    }
}

/** Default [[EdgeAnnotationCombiner]] using set union. */
given setCombiner[A]: EdgeAnnotationCombiner[A, Set[A]] with {
  def single(a: A): Set[A] = Set(a)
  def include(k: Set[A], a: A): Set[A] = k + a
  def combine(k1: Set[A], k2: Set[A]): Set[A] = k1 ++ k2
}

object Elements {
  // Use:
  // import org.maraist.fa.elements.*
  // import org.maraist.fa.annotated.Elements.*
  case class SetAnnotation[S,T,A](src: S, label: T, dest: S, annotation: A)
  case class RemoveAnnotation[S,T,A](src: S, label: T, dest: S)

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
  [S, T, NA, DA, +D <: IndexedDFA[Set[S],T] & EdgeAnnotatedDFA[Set[S],T,DA]]
  (using combo: EdgeAnnotationCombiner[NA, DA])
    extends NDFA[S, T, D] {

  /** Return the annotation (if any) on the transition from `src` to
    * `dest` labelled `label`.
    */
  def annotation(src: S, label: T, dest: S): Option[NA]

  /** Check whether there is an annotation on the transition from `src`
    * to `dest` labelled `label`.
    */
  def annotated(src: S, dest: S, label: T): Boolean =
    annotation(src, label, dest).isDefined

  /** Return the annotation (if any) on the e-transition from `src` to
    * `dest`.
    */
  def annotation(src: S, dest: S): Option[NA]

  /** Check whether there is an annotation on the unlabeled transition
    * from `src` to `dest`.
    */
  def annotated(src: S, dest: S): Boolean =
    annotation(src, dest).isDefined

  override protected def dumpTransition(src: S, label: T, dest: S): Unit = {
    print("- " + src + " -[ " + label)
    annotation(src, label, dest) match {
      case None => { print(" (unann.)") }
      case Some(a) => { print(" : " + a) }
    }
    print(" ]-> " + dest)
    println()
  }

  override protected def dumpTransition(src: S, dest: S): Unit = {
    println("- " + src + " -{ ")
    annotation(src, dest) match {
      case None => { print(" (unann.)") }
      case Some(a) => { print(" : " + a) }
    }
    print(" }-> " + dest)
  }
}

object EdgeAnnotatedNDFA {

  def newBuilder[S, T, NA, DA](using combo: EdgeAnnotationCombiner[NA, DA]):
      NDFAEdgeAnnotationsBuilder[S, T, NA, DA,
        ? <: IndexedDFA[Set[S],T] & EdgeAnnotatedDFA[Set[S],T,DA],
        ? <: EdgeAnnotatedNDFA[S,T,NA,DA,?],
        Elements.AnnotatedNDFAelement[S,T,NA]] =
    new HashEdgeAnnotatedNDFABuilder[S, T, DA, NA]

}

/** Methods to be implemented by an edge-annotated NDFA builder.
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the
  * automaton
  * @tparam NA The type of annotations on transitions in the NDFA.
  * @tparam DA The type of annotations on transitions in the DFA.
  * @tparam D Type of DFA converted from this NDFA
  *
  * @group Annotated
  */
trait NDFAEdgeAnnotationsBuilder
  [S, T, NA, DA,
    +D <: IndexedDFA[Set[S],T] & EdgeAnnotatedDFA[Set[S],T,DA],
    +N <: EdgeAnnotatedNDFA[S,T,NA,DA,D],
    E >: Elements.AnnotatedNDFAelement[S,T,NA] <: Matchable
  ]
    extends NDFABuilder[S, T, D, N, E]
    with EdgeAnnotatedNDFA[S, T, NA, DA, D] {

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
