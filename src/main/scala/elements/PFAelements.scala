// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.elements
import scala.collection.mutable.{Builder, HashMap, HashSet}

/**
  * @group PFA
  */
case class AddProbFinalState[S,T](state: S, prob: Double)

/**
  * @group PFA
  */
case class AddProbTransition[S,T](state1: S, trans: T, state2: S, prob: Double)

/**
  * @group PFA
  */
case class AddProbETransition[S,T](state1: S, state2: S, prob: Double)

/**
  * @group PFA
  */
case class RemoveProbETransition[S,T](state1: S, state2: S, prob: Double)

/**
  * @group PFA
  */
type ProbBuilders[S,T] = (
  AddProbFinalState[S,T] | AddProbTransition[S,T] | AddProbETransition[S,T]
    | RemoveProbETransition[S,T] | RemoveFinalState[S,T]
    | RemoveTransition[S,T]
)

/**
  * @group PFA
  */
type PFAelements[S, T] = AddState[S, T] | RemoveState[S, T] | ProbBuilders[S,T]
