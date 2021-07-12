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
import scala.collection.mutable.{Builder, HashMap, HashSet}
import org.maraist.fa.DFA.IndexedDFA

/** Builders for nondeterministic finite automata (NDFAs)
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @group NDFA
  */
trait NDFABuilder[
  S, T, +ThisDFA <: IndexedDFA[Set[S],T],
  +ThisNDFA <: NDFA[S,T,ThisDFA],
  K >: NDFA.NDFAelements[S,T] <: Matchable
]
    extends NDFA[S,T,ThisDFA]
    with Builder[NDFA.NDFAelements[S,T], ThisNDFA] {
  /** Adds a state to the automaton */
  def addState(s:S):Unit
  /** Removes a state from the automaton */
  def removeState(s:S):Unit
  /** Adds an initial state to the automaton */
  def addInitialState(s:S):Unit
  /** Causes a state not to be considered an initial state, but does
    * ''not'' remove it from the automaton */
  def removeInitialState(s:S):Unit
  /** Adds a final state to the automaton */
  def addFinalState(s:S):Unit
  /** Causes a state not to be considered a final state, but does
    * ''not'' remove it from the automaton */
  def removeFinalState(s:S):Unit

  /** Adds a transition labelled `t` from `s1` to `s2` */
  def addTransition(s1:S, t:T, s2:S): Unit
  /** Adds an &epsilon;-transition from `s1` to `s2` */
  def addETransition(s1:S, s2:S): Unit
  /** Removes any transition labelled `t` from `s1` to `s2` */
  def removeTransition(s1:S, t:T, s2:S): Unit
  /** Removes any &epsilon;-transition from `s1` to `s2` */
  def removeETransition(s1:S, s2:S): Unit

  /** This {@link scala.collection.mutable.Builder Builder} method
    * is not implemented at this time.
    */
  def clear(): Unit = throw new UnsupportedOperationException()

  /** Returns the (possibly immutable) [[org.maraist.fa.NDFA NDFA]]
    * described to this builder */
  def toNDFA: ThisNDFA
}

