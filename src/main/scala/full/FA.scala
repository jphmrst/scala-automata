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
import org.maraist.fa.traits
import org.maraist.fa.styles.AutomatonStyle

/** Partial implementation of an
  * [[traits.FA indexed finite automaton]] using
  * [[scala.collection.immutable.IndexedSeq `IndexedSeq`s]] and
  * `Array`s.
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam Z Type of style options for Graphviz export
  *
  * @group DFA
  */
trait FA[S, T, -Z[S, T] <: AutomatonStyle[S, T]]

extends traits.FA[S, T, Z] with UnindexedFA[S, T, Z] {

  protected val stateSeq: IndexedSeq[S]
  protected val transitionsSeq: IndexedSeq[T]

  override def size: Int = stateSeq.length

  override def states: IndexedSeq[S] = stateSeq
  override def state(i: Int): S = stateSeq(i)
  override def indexOf(s: S): Int = stateSeq.indexOf(s)
  override def isState(s: S): Boolean = stateSeq.contains(s)
  override def isInitialState(s: S): Boolean =
    finalStateIndices.contains(indexOf(s))
  override def finalStates: Set[S] = finalStateIndices.map(stateSeq)
  override def isFinalState(s: S): Boolean = {
    val si = stateSeq.indexOf(s)
    finalStateIndices.contains(si)
  }

  override def labels: IndexedSeq[T] = transitionsSeq
  override def labelIndex(t:T):Int = transitionsSeq.indexOf(t)
  override def label(i:Int):T = transitionsSeq(i)

  override def toDOT(using Z[S, T]): String = {
    val sb = new StringBuilder()
    internalsToDOT(states, labels, sb)
    sb.toString()
  }

}
