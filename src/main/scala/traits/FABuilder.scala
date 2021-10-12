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
import org.maraist.fa.styles.AutomatonStyle

/** Methods for the builder of any finite automaton.
  *
  * @tparam S Type representing states.
  * @tparam T Type representing transition labels.
  * @tparam Z Type of style options for Graphviz export
  */
trait FABuilder[S, T, -Z[S, T] <: AutomatonStyle[S, T]]

extends UnindexedFA[S, T, Z] {

  /** Adds a state to the automaton.  This method should have no effect
    * if the state is already included. */
  def addState(s: S): Unit

  /** Remove a state from the automaton.  This method should have no
    * effect if the state is not part of the automaton. */
  def removeState(s: S): Unit

  /** Adds a final state to the automaton.  This method should have no
    * effect if the state is already a final state, and should subsume
    * [[#addState]] if the state has not previously been added. */
  def addFinalState(s: S): Unit

  /** Remove a state from the set of final states of the automaton.
    * This method should have no effect if the state is not already a
    * final state, or is not part of the automaton at all. */
  def removeFinalState(s: S): Unit

  /** Adds a transition labelled `t` from `s1` to `s2` */
  def addTransition(s1: S, t: T, s2: S): Unit
}
