// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.full
import scala.collection.mutable.{HashMap, HashSet}
import org.maraist.fa.util.EdgeAnnotationCombiner
import org.maraist.fa.elements.*
import org.maraist.fa.styles.AutomatonStyle
import org.maraist.fa.traits

/** Partial implementation of a builder for edge-annotated NFAs using
  * [[scala.collection.mutable.HashSet `HashSet`s]] and
  * [[scala.collection.mutable.HashMap `HashMap`s]].
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam D Type of DFA constructed by this builder.
  * @tparam K Builder elements for this builder.
  * @tparam Z Type of style options for Graphviz export
  *
  * @group DFA
  */
trait EdgeAnnotatedNFABuilder[
  S, T, NA, DA,
  G[X] <: Set[X],
  +D[DS, DT, DDA] <: EdgeAnnotatedDFA[DS, DT, DDA, Z],
  +N[NS, NT, NNA, NDA] <: EdgeAnnotatedNFA[NS, NT, NNA, NDA, G, D, Z],
  -K >: EdgeAnnotatedNFAelements[S, T, NA] <: Matchable,
  -Z[ZS, ZT] <: AutomatonStyle[ZS, ZT]
](using combiner: EdgeAnnotationCombiner[NA, DA])

extends NFABuilder[
  S, T, G,
  [DS, DT] =>> D[DS, DT, DA],
  [DS, DT] =>> N[DS, DT, NA, DA],
  K, Z
]

with traits.UnindexedEdgeAnnotatedNFA[S, T, NA, DA, G, D, Z]

with traits.EdgeAnnotatedNFABuilder[S, T, NA, DA, G, D, N, K, Z] {

  protected val labelledEdgeAnnotations:
      HashMap[S, HashMap[T, HashMap[S, NA]]] =
    new HashMap[S, HashMap[T, HashMap[S, NA]]]

  def annotation(src: S, label: T, dest: S): Option[NA] =
    labelledEdgeAnnotations.get(src).map(_(label)).map(_(dest))

  def setAnnotation(src: S, label: T, dest: S, annotation: NA): Unit =
    labelledEdgeAnnotations.get(src) match {
      case None => {
        labelledEdgeAnnotations(src) =
          HashMap(label -> HashMap(dest -> annotation))
      }
      case Some(submap1) => submap1.get(label) match {
        case None => { submap1(label) = HashMap(dest -> annotation) }
        case Some(submap2) => { submap2(dest) = annotation }
      }
    }

  def removeAnnotation(src: S, label: T, dest: S): Unit =
    labelledEdgeAnnotations.get(src) match {
      case None => { }
      case Some(submap1) => submap1.get(label) match {
        case None => { }
        case Some(submap2) => {
          submap2 -= dest
          if (submap2.isEmpty) {
            submap1 -= label
            if (submap1.isEmpty) {
              labelledEdgeAnnotations -= src
            }
          }
        }
      }
    }

  override def annotated(src: S, label: T, dest: S): Boolean =
    annotation(src, label, dest).isDefined

  def annotation(src: S, dest: S): Option[NA] =
    unlabelledEdgeAnnotations.get(src).flatMap(_.get(dest))

  protected val unlabelledEdgeAnnotations: HashMap[S, HashMap[S, NA]] =
    new HashMap[S, HashMap[S, NA]]

  override def eAnnotated(src: S, dest: S): Boolean =
    eAnnotation(src, dest).isDefined

  override def eAnnotation(src: S, dest: S): Option[NA] =
    unlabelledEdgeAnnotations.get(src).flatMap(_.get(dest))

  def setEAnnotation(src: S, dest: S, annotation: NA): Unit =
    unlabelledEdgeAnnotations.get(src) match {
      case None => {
        unlabelledEdgeAnnotations(src) = HashMap(dest -> annotation)
      }
      case Some(submap) => { submap(dest) = annotation }
    }

  def removeEAnnotation(src: S, dest: S): Unit =
    unlabelledEdgeAnnotations.get(src) match {
      case None => { }
      case Some(submap) => {
        submap -= dest
        if (submap.isEmpty) then unlabelledEdgeAnnotations -= src
      }
    }

  override def addOne(elem: K): this.type = {
    elem match {
      case SetAnnotation(src, label, dest, annotation): SetAnnotation[S,T,NA] =>
        setAnnotation(src, label, dest, annotation)
      case RemoveAnnotation(src, label, dest): RemoveAnnotation[S,T,NA] =>
        removeAnnotation(src, label, dest)
      case SetEAnnotation(src, dest, annotation): SetEAnnotation[S, NA] =>
        setEAnnotation(src, dest, annotation)
      case RemoveEAnnotation(src, dest): RemoveEAnnotation[S, NA] =>
        removeEAnnotation(src, dest)
      case _ => super.addOne(elem)
    }
    this
  }

  override protected def assembleNFA(
    statesSeq: IndexedSeq[S],
    initials: Set[Int],
    finals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    labelsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]]
  ): N[S, T, NA, DA] = {
    assembleNFA(
      statesSeq, initials, finals, transitionsSeq,
      labelsArray.map(_.map(_.toSet)),
      epsilonsArray.map(_.toSet),
      Array.tabulate(statesSeq.size, transitionsSeq.size, statesSeq.size)(
        (s1, t, s2) => labelledEdgeAnnotations.get(statesSeq(s1))
          .flatMap(_.get(transitionsSeq(t)))
          .flatMap(_.get(statesSeq(s2)))),
      Array.tabulate(statesSeq.size, statesSeq.size)(
        (s1, s2) => unlabelledEdgeAnnotations.get(statesSeq(s1))
          .flatMap(_.get(statesSeq(s2)))))
  }

  protected def assembleNFA(
    statesSeq: IndexedSeq[S],
    initials: Set[Int],
    finals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    labelsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]],
    labelledEdgeAnnotations: Array[Array[Array[Option[NA]]]],
    unlabelledEdgeAnnotations: Array[Array[Option[NA]]]):
      N[S, T, NA, DA]
}
