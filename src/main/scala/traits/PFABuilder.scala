// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.traits
import scala.collection.mutable.Builder
import org.maraist.fa.elements
import org.maraist.fa.styles.ProbabilisticAutomatonStyle

/** Builders for probabilistic finite automata (PFAs).
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam A Type of automaton constructed.
  * @tparam Z Type of style options for Graphviz export.
  * @group PFA
  */
trait PFABuilder[
  S, T,
  +A[DS, DT] <: PFA[DS, DT, Z],
  -K >: elements.PFAelements[S, T] <: Matchable,
  -Z[S, T] <: ProbabilisticAutomatonStyle[S, T]]

extends Builder[K, A[S, T]]

with UnindexedPFA[S, T, Z] {

  /** Adds a state to the automaton */
  def addState(s: S): Unit
  /** Removes a state from the automaton */
  def removeState(s: S): Unit

  /** Adds a state to the automaton */
  def addInitialState(s: S, prob: Double): Unit
  /** Removes a state from the automaton */
  def removeInitialState(s: S): Unit

  /** Adds a state to the automaton */
  def addFinalState(s: S, prob: Double): Unit
  /** Removes a state from the automaton */
  def removeFinalState(s: S): Unit

  /** Adds a transition labelled `t` from `s1` to `s2`, removing any
   *  previous transition labelled `t` from `s1` to `s2`.
   */
  def addTransition(s1: S, t: T, s2: S, prob: Double): Unit
  /** Removes any transition labelled `t` from `s1` to `s2` */
  def removeTransition(s1: S, t: T, s2: S): Unit

  /** Adds an epsilon transition from `s1` to `s2`, removing any
   *  previous epsilon transition from `s1` to `s2`.
   */
  def addETransition(s1: S, s2: S, prob: Double): Unit
  /** Removes any epsilon transition from `s1` to `s2` */
  def removeETransition(s1: S, s2: S): Unit

  def removeEpsilonTransitions:Unit
//   = new EpsilonRemover(this).run()

//  override def addOne(builder: PFAelements[S,T]): this.type = {
//    builder match {
//      case AddState(s): AddState[S, T] => addState(s)
//      case RemoveState(state) => removeState(state)
//      case AddProbFinalState(state, prob) => addFinalState(state, prob)
//      case RemoveFinalState(state) => removeFinalState(state)
//      case AddProbTransition(state1, trans, state2, prob) =>
//        addTransition(state1, trans, state2, prob)
//      case RemoveTransition(state1, trans, state2): RemoveTransition[S, T] =>
//        removeTransition(state1, trans, state2)
//      case SetInitialState(state) => SetInitialState(state)
//      case AddProbETransition(state1, state2, prob) =>
//        addETransition(state1, state2, prob)
//      case RemoveProbETransition(state1, state2, prob) =>
//        removeETransition(state1, state2)
//    }
//    this
//  }
//
//  /** This {@link scala.collection.mutable.Builder Builder} method
//    * is not implemented at this time.
//    */
//  def clear(): Unit = throw new UnsupportedOperationException()
}
