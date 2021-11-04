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
import org.typelevel.paiges.Doc
import scala.collection.mutable.{HashMap,HashSet}
import org.maraist.fa.elements.{FAelements,
  AddState, RemoveState, AddFinalState, RemoveFinalState}
import org.maraist.fa.styles.AutomatonStyle
import org.maraist.fa.traits

/** Implementation of a builder for some automaton type using
  * [[scala.collection.mutable.HashSet `HashSet`s]] and
  * [[scala.collection.mutable.HashMap `HashMap`s]].
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam D Type of automaton constructed by this builder.
  * @tparam K Builder elements for this builder.
  * @tparam Z Type of style options for Graphviz export
  */
trait FABuilder[
  S, T,
  +A[AS, AT] <: FA[AS, AT, Z],
  -K >: FAelements[S, T] <: Matchable, // TODO revisit DFA vs. FA elements
  -Z[X, Y] <: AutomatonStyle[X, Y]
]

extends traits.FABuilder[S, T, A, K, Z]

with UnindexedFA[S, T, Z] with StatesMixin[S, T] {

  /** Storage for all final state objects */
  protected val finalStatesSet: HashSet[S] = new HashSet[S]

  /** Clear this builder.  When overriding this method, it is important
    * to call `super.clear()`.
    */
  override def clear(): Unit = {
    super.clear()
    finalStatesSet.clear()
  }

  /** Internal, low-level method for removing all transitions emerging
    * from a particular state.  Does no consistency checking.  */
  override protected def deleteTransitionsFrom(s: S): Unit

  override def removeFinalState(s: S): Unit = { finalStatesSet -= s }

  override def addFinalState(s: S): Unit = {
    finalStatesSet += s
    addState(s)
  }

  override def finalStates: Set[S] = Set.from(finalStatesSet)
  override def isFinalState(s: S): Boolean = finalStatesSet.contains(s)

  override def addOne(elem: K): this.type = {
    elem match {
      case AddState(s): AddState[S,T] => addState(s)
      case RemoveState(s): RemoveState[S,T] => removeState(s)
      case RemoveFinalState(s): RemoveFinalState[S,T] => removeFinalState(s)
      case AddFinalState(s): AddFinalState[S,T] => addFinalState(s)
    }
    this
  }

  override protected def prettyHeader: Doc =
    Doc.text("---------- FABuilder dump")
}
