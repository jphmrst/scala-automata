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

/** Methods for builders of a nondeterministic finite automaton.
  *
  * @tparam S Type representing states.
  * @tparam T Type representing transition labels.
  * @tparam D Type of DFA constructed by the resulting NFA.
  * @tparam N Type of NFA constructed by this builder.
  * @tparam G Collection type constructor for states in the DFA
  * constructed by the resulting NFA.
  * @tparam Z Type of style options for Graphviz export
  */
trait NFABuilder[
  S, T,
  G[X] <: Set[X],
  +D[DS, DT] <: DFA[S, T, Z],
  +N[NS, NT] <: NFA[S, T, G, D, Z],
  -Z[S, T] <: AutomatonStyle[S, T]]

extends FABuilder[S, T, Z] with UnindexedNFA[S, T, G, D, Z] {

  /** Add an initial state to the NFA. */
  def addInitialState(s: S): Unit

  /** Remove an initial state from the NFA. */
  def removeInitialState(s: S): Unit

  /** Removes any transition labelled `t` from `s1` to `s2` */
  def removeTransition(s1: S, t: T, s2: S):Unit

  /** Adds an &epsilon;-transition from `s1` to `s2` */
  def addETransition(s1: S, s2: S): Unit

  /** Removes any &epsilon;-transition from `s1` to `s2` */
  def removeETransition(s1: S, s2: S): Unit
}
