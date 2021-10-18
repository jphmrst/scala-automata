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
import org.maraist.fa.styles.ProbabilisticAutomatonStyle

/** Trait of the basic usage operations on any PFA.
 *
 *  @tparam S The type of all states of the automaton.
 *  @tparam T The type of labels on transitions of the automaton.
  * @tparam Z Type of style options for Graphviz export.
 *
 * @group PFA
 */
trait UnindexedPFA[S, T, -Z[S, T] <: ProbabilisticAutomatonStyle[S, T]]

extends UnindexedFA[S, T, Z] {

  /** Returns the probability that `s` is an initial state of this
    * automaton.
   */
  def initialStateProb(s: S): Double

  /** Returns the probability that `s` is a final state of this
    * automaton.
    */
  def finalStateProb(s: S): Double

  /** Returns `true` if `s` has a non-zero chance of being a final state
   *  of the automaton
   */
  def isFinalState(s: S): Boolean

  /** Calculate the probability that the automaton accepts the given sequence
   */
  def acceptsProb(ts: Seq[T]): Double

  /** Returns the states into which the automaton could transition with
    * non-zero probability starting from `s` via a transition labelled
    * `t`.
    *
    * In the resulting map, each resulting state is associated with
    * the probability of that transition taking place.
    */
  def possibleTransitions(s: S, t: T): Map[S, Double]

  /** Returns the probability that from state s0 and with label t, this state
    * would transition into state s1.
    */
  def transitionProb(s0: S, t: T, s1: S): Double

  /** Returns the probability of a transition state s0 into state s1 without
    *  a labeled event.
    */
  def eTransitionProb(s0:S, s1:S): Double

  /** For normalization, returns the total of the probabilities
    * associated with each state/label pair.
    */
  protected[fa] def getEdgeProbTotals: Map[S, Map[T, Double]]

  /** Perform some action for each state in the automaton. */
  def foreachState(action: (s: S, prob: Double) => Unit): Unit

  /** Perform some action for each initial state in the automaton. */
  def foreachInitialState(action: (s: S, prob: Double) => Unit): Unit

  /** Perform some action for each final state in the automaton. */
  def foreachFinalState(action: (s: S, prob: Double) => Unit): Unit

  /** Perform some action for each transition in the automaton. */
  def foreachTransition(action: (s1: S, t: T, s2: S, prob: Double) => Unit):
      Unit

  /** Perform some action for each epsilon transition in the
    * automaton. Note that in many (deterministic) automata, this
    * method is a no-op, but in included for an easy consistency. */
  def foreachETransition(action: (s1: S, s2: S, prob: Double) => Unit): Unit
}
