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

/** Partial implementation of a [[traits.DFA DFA]] storing transition
  * information in `Array`s.
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam Z Type of style options for Graphviz export
 *
 * @group DFA
 */
trait UnindexedDFA[S, T, -Z[S, T] <: AutomatonStyle[S, T]]

extends traits.UnindexedDFA[S, T, Z] with UnindexedFA[S, T, Z] {

  override val initialStates: Set[S] = Set(initialState)
  override def transitions(s: S, t: T): Set[S] = Set.from(transition(s, t))

  override def foreachETransition(action: (s1: S, s2: S) => Unit): Unit = { }

  import scala.util.control.NonLocalReturns.*
  override def accepts(string: Seq[T]): Boolean = returning {
    var current = initialState
    for(t <- string) {
      transition(current, t) match {
        case Some(s) => { current = s }
        case None => throwReturn(false)
      }
    }
    finalStates.contains(current)
  }

}
