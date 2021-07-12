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

/** Methods associated with the states of an automaton.
  * @tparam S Type representing states.
  */
trait StateHolder[S] {
  /** The states themselves */
  def states: Iterable[S]

  /** Number of states in the automaton */
  def size: Int

  /** Returns `true` if `s` is used as a state */
  def isState(s: S): Boolean
}

trait IndexedStateHolder[S] extends StateHolder[S] {
  override def states: IndexedSeq[S]

  /** Return the state at a particular index. */
  def state(i: Int): S

  /** Return the index of a state. */
  def indexOf(s: S): Int
}

trait FinalStateSetHolder[S] {
  def finalStates: Set[S]

  def isFinalState(s:S):Boolean
}

trait IndexedFinalStateSetHolder[S] extends FinalStateSetHolder[S] {
  def finalStateIndices: Set[Int]
}

trait InitialStateSetHolder[S] {
  def getInitialStates: Set[S]
  def isInitialState(s: S): Boolean
}

trait IndexedInitialStateSetHolder[S] extends InitialStateSetHolder[S] {
  def initialStateIndices: Set[Int]
}

trait SingleInitialStateHolder[S] extends InitialStateSetHolder[S] {
  def getInitialState: S
  def getInitialStates: Set[S] = Set(getInitialState)
}

trait IndexedSingleInitialStateHolder[S]
    extends IndexedInitialStateSetHolder[S]
    with SingleInitialStateHolder[S] {
  this: IndexedStateHolder[S] =>
  def initialStateIndex: Int
  def initialStateIndices: Set[Int] = Set(initialStateIndex)
}

trait LabelsHolder[T] {
  /** Set of automaton transition labels */
  def labels: Iterable[T]
}

trait IndexedLabelsHolder[T] extends LabelsHolder[T] {
  override def labels: IndexedSeq[T]
  def labelIndex(t:T): Int
  def label(i:Int): T
}
