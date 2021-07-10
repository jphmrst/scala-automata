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
import org.maraist.fa.impl.AbstractArrayDFA

/** Implementation of a [[org.maraist.fa.DFA DFA]] using
 *  [[scala.collection.immutable.IndexedSeq `IndexedSeq`s]] and
 *  `Array`s.
 * @param initialStateIndex Index of the initial state of the automaton.
 * @param finalStateIndices Set of the indices of the final states of the
 * automaton
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on (non-epsilon) transitions of the automaton
 *
 * @group DFA
 */
abstract class AbstractEdgeAnnotatedArrayDFA[S, T, A](
  stateSeq: IndexedSeq[S],
  initialStateIndex: Int,
  finalStateIndices: Set[Int],
  transitionsSeq: IndexedSeq[T],
  transitionsMatrix: Array[Array[Int]],
  val edgeAnnotations: Array[Array[Option[A]]]
)
    extends AbstractArrayDFA[S,T](
      stateSeq, initialStateIndex, finalStateIndices,
      transitionsSeq, transitionsMatrix
    )
    with DFAEdgeAnnotations[S,T,A] {

  /** Return the annotation (if any) on the transition from `src`
    * labelled `label`.
    */
  def annotation(src: S, label: T): Option[A] =
    annotationIndex(indexOf(src), labelIndex(label))

  /** Return the annotation (if any) on the transition from the state at
    * index `srcIdx` with the label with index `labelIdx`.
    */
  def annotationIndex(srcIdx: Int, labelIdx: Int): Option[A] =
    edgeAnnotations(srcIdx)(labelIdx)
}

class EdgeAnnotatedArrayDFA[S,T,A](
  stateSeq: IndexedSeq[S],
  initialStateIndex: Int,
  finalStateIndices: Set[Int],
  transitionsSeq: IndexedSeq[T],
  transitionsMatrix: Array[Array[Int]],
  edgeAnnotations: Array[Array[Option[A]]]
)
extends AbstractEdgeAnnotatedArrayDFA[S,T,A](
  stateSeq, initialStateIndex, finalStateIndices,
  transitionsSeq, transitionsMatrix, edgeAnnotations
) {
  type Traverser = org.maraist.fa.DFA.DFAtraverser[S,T]
  protected def dotTraverser(sb:StringBuilder,stateList:IndexedSeq[S]) = ???
}
