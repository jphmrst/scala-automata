// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.impl
import org.maraist.graphviz.{GraphStyle}
import scala.collection.mutable.HashSet
import org.maraist.fa.elements.*
import org.maraist.fa.DFA
import org.maraist.fa.DFA.*

/**
 * Concrete builder class for {@link org.maraist.fa.DFA DFAs} based on hash
 * tables.
 *
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on transitions of the automaton
 *
 *  @group DFA
 */
class HashDFABuilder[S,T](initialState: S)
extends AbstractHashDFABuilder[
  S, T,
  AbstractArrayDFA[S,T],
  DFAelements[S,T]
](initialState) {

  type ThisDFA = ArrayDFA[S,T]
  type Traverser = DFAtraverser[S,T, ? >: this.type]
  protected def dotTraverser(sb:StringBuilder,stateList:IndexedSeq[S]) =
    new DotTraverseDFA[S, T, this.type](
      summon[GraphStyle[S, T]], sb, stateList, initialState)
  protected def assembleDFA(statesSeq: IndexedSeq[S],
                            initialIdx: Int,
                            finalStateIndices: HashSet[Int],
                            transitionsSeq: IndexedSeq[T],
                            idxLabels: Array[Array[Int]]): ThisDFA = {
    new ArrayDFA[S,T](statesSeq, initialIdx, finalStateIndices.toSet,
                      transitionsSeq, idxLabels)
  }
}
