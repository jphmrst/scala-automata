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

/** Contains only the addState method; used as the self-type of traits
  * which require this method.
  */
trait StateAdder[S] {
  def addState(s: S): Unit
}

trait StateBuilder[S] extends StateAdder[S] {
  def removeState(s: S): Unit
}

trait FinalStateSetBuilder[S] {
  def addFinalState(s: S): Unit
  def removeFinalState(s: S): Unit
}

trait SingleInitialStateBuilder[S] {
  def setInitialState(s: S): Unit
}

trait InitialStateSetBuilder[S] {
  def addInitialState(s: S): Unit
  def removeInitialState(s: S): Unit
}

trait DeterministicLabelledTransitionBuilder[S, T] {
  /** Adds a transition labelled `t` from `s1` to `s2` */
  def addTransition(s1: S, t: T, s2: S): Unit
  /** Removes the transition labelled `t` from `s1`, if any. */
  def removeTransition(s1:S, t:T):Unit
}

trait NondeterministicLabelledTransitionBuilder[S, T] {
  /** Adds a transition labelled `t` from `s1` to `s2` */
  def addTransition(s1: S, t: T, s2: S): Unit
  /** Removes any transition labelled `t` from `s1` to `s2` */
  def removeTransition(s1: S, t: T, s2: S):Unit
}

trait UnlabelledTransitionBuilder[S] {
  /** Adds an &epsilon;-transition from `s1` to `s2` */
  def addETransition(s1:S, s2:S): Unit
  /** Removes any &epsilon;-transition from `s1` to `s2` */
  def removeETransition(s1:S, s2:S): Unit
}