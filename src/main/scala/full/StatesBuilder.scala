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
import scala.collection.mutable.HashSet

/** Mixin bringing implementations of adding/removing states using
  * [[scala.collection.mutable.HashSet `HashSet`s]] and
  * [[scala.collection.mutable.HashMap `HashMap`s]].
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  */
trait StatesMixin[S, T] {

  /** Storage for all state objects */
  protected val allStates: HashSet[S] = new HashSet[S]

  protected def removeFinalState(s: S): Unit

  protected def deleteTransitionsFrom(s: S): Unit

  def clear(): Unit = allStates.clear()

  def addState(s:S):Unit = { allStates += s }

  def removeState(s:S):Unit = {
    allStates -= s
    removeFinalState(s)
    // TODO --- what if it's initial // removeInitialState(s)
    deleteTransitionsFrom(s)
  }
  def size: Int = allStates.size

  def states: Set[S] = Set.from(allStates)

  def isState(s:S): Boolean = allStates.contains(s)
}
