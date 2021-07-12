// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.annotated
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.{HashMap, HashSet}
import org.maraist.util.IndexSetsTracker
import org.maraist.fa.NDFA
import org.maraist.fa.NDFA.NDFAelements
import org.maraist.fa.impl.
  {AbstractArrayDFA, AbstractArrayNDFA, AbstractHashNDFABuilder}

abstract class AbstractEdgeAnnotatedArrayNDFA
  [S, T, +ThisDFA <: AbstractEdgeAnnotatedArrayDFA[Set[S],T,A],
    A <: Matchable]
  (stateSeq: IndexedSeq[S],
    initialStateSet: Set[Int],
    finalStateSet: Set[Int],
    transitionsSeq: IndexedSeq[T],
    transitionsArray: Array[? <: Array[? <: Set[Int]]],
    epsilons: Array[? <: Set[Int]],
    protected val labelledEdgeAnnotations:
        Array[? <: Array[? <: Array[? <: Option[A]]]],
    protected val unlabelledEdgeAnnotations:
        Array[? <: Array[? <: Option[A]]])
extends AbstractArrayNDFA[S, T, ThisDFA]
  (stateSeq, initialStateSet, finalStateSet,
    transitionsSeq, transitionsArray, epsilons) {

  /** Return the annotation (if any) on the transition from `src` to
    * `dest` labelled `label`.
    */
  def annotation(src: S, label: T, dest: S): Option[A] =
    annotationIndex(indexOf(src), labelIndex(label), indexOf(src))

  /** Return the annotation (if any) on the transition from the state at
    * index `srcIdx` with the label with index `labelIdx`.
    */
  def annotationIndex(srcIdx: Int, labelIdx: Int, destIdx: Int): Option[A] =
    labelledEdgeAnnotations(srcIdx)(labelIdx)(destIdx)

  /** Return the annotation (if any) on the unlabelled transition from
    * `src` to `dest`.
    */
  def annotation(src: S, dest: S): Option[A] =
    annotationIndex(indexOf(src), indexOf(src))

  /** Return the annotation (if any) on the unlabelled transition from
    * the state at index `srcIdx`.
    */
  def annotationIndex(srcIdx: Int, destIdx: Int): Option[A] =
    unlabelledEdgeAnnotations(srcIdx)(destIdx)

  protected def assembleDFA(
    dfaStates:IndexedSeq[Set[S]],
    initialStateIdx:Int,
    dfaFinals:Set[Int],
    transitionsSeq:IndexedSeq[T],
    dfaTransitions:Array[Array[Int]],
    tracker:IndexSetsTracker,
    appearsIn: Array[Set[Int]],
    edgeAnnotations: Array[Array[Option[A]]]): ThisDFA

  override protected def assembleDFA(
    dfaStates: IndexedSeq[Set[S]],
    initialStateIdx: Int,
    dfaFinals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    dfaTransitions: Array[Array[Int]],
    tracker: IndexSetsTracker,
    appearsIn: Array[Set[Int]]):
      ThisDFA = {
    val edgeAnnotations: Array[Array[Option[A]]] = ???
    assembleDFA(
      dfaStates, initialStateIdx, dfaFinals, transitionsSeq,
      dfaTransitions, tracker, appearsIn, edgeAnnotations
    )
  }
}

class EdgeAnnotatedArrayNDFA[S,T,A <: Matchable](
  stateSeq: IndexedSeq[S],
  initialStateSet: Set[Int],
  finalStateIndices: Set[Int],
  transitionsSeq: IndexedSeq[T],
  transitionsMatrix: Array[Array[Set[Int]]],
  epsilons: Array[Set[Int]],
  labelledEdgeAnnotations: Array[Array[Array[Option[A]]]],
  unlabelledEdgeAnnotations: Array[Array[Option[A]]]
)
extends AbstractEdgeAnnotatedArrayNDFA
  [S, T, EdgeAnnotatedArrayDFA[Set[S],T,A], A]
  (stateSeq, initialStateSet, finalStateIndices,
    transitionsSeq, transitionsMatrix, epsilons,
    labelledEdgeAnnotations, unlabelledEdgeAnnotations
  ) {

  // TODO
  protected def dotTraverser(sb:StringBuilder,stateList:IndexedSeq[S]) = ???

  protected def assembleDFA(
    dfaStates: IndexedSeq[Set[S]],
    initialStateIdx: Int,
    dfaFinals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    dfaTransitions: Array[Array[Int]],
    tracker: IndexSetsTracker,
    appearsIn: Array[Set[Int]],
    edgeAnnotations: Array[Array[Option[A]]]):
      EdgeAnnotatedArrayDFA[Set[S],T,A] =
    new EdgeAnnotatedArrayDFA[Set[S],T,A](
      dfaStates, initialStateIdx, dfaFinals, transitionsSeq,
      dfaTransitions, edgeAnnotations)
}
