// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa
import org.maraist.graphviz.{GraphStyle}
import scala.collection.mutable.HashSet
import org.maraist.fa.styles.AutomatonStyle
import org.maraist.fa.elements.*
import org.maraist.fa.full

/**
 * Concrete builder class for {@link org.maraist.fa.DFA DFAs} based on hash
 * tables.
 *
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on transitions of the automaton
 *
 *  @group DFA
 */
class DFABuilder[S,T](init: S)

extends full.DFABuilder[S, T, DFA, DFAelements[S, T], AutomatonStyle](init) {

  protected def assembleDFA(statesSeq: IndexedSeq[S],
                            initialIdx: Int,
                            finalStateIndices: HashSet[Int],
                            transitionsSeq: IndexedSeq[T],
                            idxLabels: Array[Array[Int]]): DFA[S, T] = {
    val result = new DFA[S, T](
      statesSeq, initialIdx, finalStateIndices.toSet, transitionsSeq,
      idxLabels)
    result
  }
}
