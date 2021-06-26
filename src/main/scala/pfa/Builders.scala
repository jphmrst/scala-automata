// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.pfa
import scala.collection.mutable.{Builder, HashMap, HashSet}
import org.maraist.fa.general.Builders.*
import org.maraist.fa.DFA.SingleInitialStateBuilders
import org.maraist.fa.pfa.PFA
import org.maraist.fa.pfa.impl.HashPFABuilder

/**
  * @group PFA
  */
object Builders {

  case class AddProbFinalState[S,T](state: S, prob: Double)
  case class AddProbTransition[S,T](state1: S, trans: T, state2: S, prob: Double)
  case class AddProbETransition[S,T](state1: S, state2: S, prob: Double)
  case class RemoveProbETransition[S,T](state1: S, state2: S, prob: Double)
  type ProbBuilders[S,T] = AddProbFinalState[S,T] | AddProbTransition[S,T] | AddProbETransition[S,T] | RemoveProbETransition[S,T]

  type PFAelements[S, T] =
    SingleInitialStateBuilders[S] | ProbBuilders[S,T] | AnyBuilders[S,T]

  given HasBuilder[PFAelements, HashPFABuilder, PFA] with {
    override def build[S,T](): HashPFABuilder[S, T] = new HashPFABuilder[S, T]
  }
}
