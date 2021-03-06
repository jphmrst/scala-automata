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
import scala.collection.mutable.{Builder, HashMap, HashSet, Queue}
import org.maraist.fa.util.IndexSetsTracker
import org.maraist.fa.styles.AutomatonStyle
import org.maraist.fa.traits

/** Methods provided by nondeterministic finite automata (NFAs).
  *
  * Trait specifying methods provided by all NFAs, and providing
  * default implementations for derivations from the core methods.
  *
  * @tparam S The type of all states of the automaton.
  * @tparam T The type of labels on (non-epsilon) transitions of the
  * automaton.
  *
  * @group NFA
  */
class NFA[S, T](
  override val stateSeq: IndexedSeq[S],
  override val transitionsSeq: IndexedSeq[T],
  override val initialStateIndices: Set[Int],
  override val finalStateIndices: Set[Int],
  override val epsilonsArray: Array[Set[Int]],
  override val transitionsArray: Array[Array[Set[Int]]]
)

extends full.NFA[S, T, Set, DFA, AutomatonStyle, AutomatonStyle] {

  checkState

  override protected def assembleDFA(
    dfaStates: IndexedSeq[Set[S]], initialStateIdx: Int, dfaFinals: Set[Int],
    transitionsSeq: IndexedSeq[T], dfaTransitions: Array[Array[Int]],
    tracker: IndexSetsTracker,
    appearsIn: Array[Set[Int]]):
      DFA[Set[S], T] =
    new DFA(
      dfaStates, initialStateIdx, dfaFinals, transitionsSeq, dfaTransitions)

  override def derivedNFA[S0, T0](
    stateSeq: IndexedSeq[S0],
    transitionsSeq: IndexedSeq[T0],
    transitionsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]],
    finalStateIndices: Set[Int],
    initialStateIndices: Set[Int]):
      NFA[S0, T0] =
    new NFA[S0, T0](
      stateSeq, transitionsSeq, initialStateIndices,
      finalStateIndices, epsilonsArray, transitionsArray)
}

object NFA {
  def newBuilder[S, T] = new NFABuilder[S,T]
}

