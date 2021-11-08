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
import org.maraist.graphviz.Graphable
import org.maraist.fa.styles.AutomatonStyle

/** Core methods of any automaton.
  * @tparam S Type representing states.
  * @tparam T Type representing transition labels.
  * @tparam Z Type of style options for Graphviz export
  */
trait UnindexedFA[S, T, -Z[S, T] <: AutomatonStyle[S, T]]

extends Graphable[S, T, Z] with org.maraist.fa.util.Pretty {

  /** The states themselves */
  def states: Iterable[S]

  /** Number of states in the automaton */
  def size: Int

  /** Returns `true` if `s` is used as a state */
  def isState(s: S): Boolean

  /** Returns the [[Set]] of final states in this automaton. */
  def finalStates: Set[S]

  /** Returns whether the given state `s` is final in this automaton. */
  def isFinalState(s: S): Boolean

  /** Returns the [[Set]] of initial states in this automaton. */
  def initialStates: Set[S]

  /** Returns whether the given state `s` is initial in this automaton. */
  def isInitialState(s: S): Boolean

  /** Set of automaton transition labels */
  def labels: Iterable[T]

  /** Returns the (possibly empty, and in deterministic implementations
    * at most singleton) set of states into which the automaton could
    * transition starting from `s` via a transition labelled `t`.
    */
  def transitions(s: S, t: T): Set[S]

  /** Perform some action for each state in the automaton. */
  def foreachState(action: (s: S) => Unit): Unit

  /** Perform some action for each initial state in the automaton. */
  def foreachInitialState(action: (s: S) => Unit): Unit

  /** Perform some action for each final state in the automaton. */
  def foreachFinalState(action: (s: S) => Unit): Unit

  /** Return the triples comprising all labelled transitions. */
  def transitionTriples: Iterable[(S, T, S)]

  /** Perform some action for each transition in the automaton. */
  def foreachTransition(action: (s1: S, t: T, s2: S) => Unit): Unit

  /** Return the pairs comprising all unlabelled transitions. */
  def eTransitionPairs: Iterable[(S, S)]

  /** Perform some action for each epsilon transition in the
    * automaton. Note that in many (deterministic) automata, this
    * method is a no-op, but in included for an easy consistency. */
  def foreachETransition(action: (s1: S, s2: S) => Unit): Unit

  /** Returns `true` if this automaton accepts the given string. */
  def accepts(string: Seq[T]): Boolean

// TODO MAP Put these back in

//  /** Derive a new isomorphic automaton from this one by applying
//    * transforms to the states and transitions.  This method does not
//    * check that either map is injective: if they are not, then the
//    * result may be unpredictable.
//    */
//  def map[S2, T2](stateMap: S => S2, transitionMap: T => T2):
//      UnindexedFA[S2, T2, Z]
//
//  /** Derive a new isomorphic automaton from this one by applying
//    * transforms to the states.  This method does not check that the
//    * `stateMap` is injective: if it is not, then the result may have
//    * separate states which cannot be distinguished.
//    */
//  def mapStates[S2](stateMap: S => S2): UnindexedFA[S2, T, Z]
//
//  /** Derive a new isomorphic automaton from this one by applying
//    * transforms to the transitions.  This method does not check that the
//    * `transitionMap` is injective: if it is not, then the result may have
//    * separate transitions which cannot be distinguished.
//    */
//  def mapTransitions[T2](transitionMap: T => T2): UnindexedFA[S, T2, Z]

}
