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
import scala.collection.mutable.HashSet
import org.maraist.fa.general.Builders.*
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
    extends AbstractHashDFABuilder[S,T, AbstractArrayDFA[S,T]](initialState) {
  type ThisDFA = ArrayDFA[S,T]
  type Traverser = DFAtraverser[S,T]
  protected def dotTraverser(sb:StringBuilder,stateList:IndexedSeq[S]) =
    new DotTraverseDFA[S,T](graphvizOptions, sb, nodeLabeling,
                            transitionLabeling, stateList, initialState)
  protected def assembleDFA(statesSeq: IndexedSeq[S],
                            initialIdx: Int,
                            finalStateIndices: HashSet[Int],
                            transitionsSeq: IndexedSeq[T],
                            idxLabels: Array[Array[Int]]): ThisDFA = {
    new ArrayDFA[S,T](statesSeq, initialIdx, finalStateIndices.toSet,
                      transitionsSeq, idxLabels)
  }

  /** Primary {@link scala.collection.mutable.Builder Builder} method
    * implementation.
    */
  override def addOne(builder: DFAelements[S, T]): this.type = {
    builder match {
      case AddState(s) => addState(s)
      case RemoveState(state) => removeState(state)
      case AddFinalState(state) => addFinalState(state)
      case RemoveFinalState(state) => removeFinalState(state)
      case AddTransition(state1, trans, state2) =>
        addTransition(state1, trans, state2)
      case RemoveTransition(state, trans, state2) => removeTransition(state, trans)
      case SetInitialState(state) => setInitialState(state)
    }
    this
  }
}
