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
import org.maraist.fa.styles.ProbabilisticAutomatonStyle

/** Trait of the usage operations on an indexed PFA.
 *
 *  @tparam S The type of all states of the automaton.
 *  @tparam T The type of labels on transitions of the automaton.
  * @tparam Z Type of style options for Graphviz export.
 *
 * @group PFA
 */
trait PFA[S, T, -Z[S, T] <: ProbabilisticAutomatonStyle[S, T]]

extends UnindexedPFA[S, T, Z]

with FA[S, T, Z] {

  def transitionIndexProb(fromIdx: Int, labelIdx: Int, toIdx: Int): Double

  def possibleTransitionsIndex(s0: Int, t: Int): Map[Int,Double]

  def eTransitionIndexProb(s0: Int, s1: Int): Double

  def initialStateIndexProb(s: Int): Double

  def finalStateIndexProb(s: Int): Double

  def initialStateIndices: Set[Int]
//  = {
//    val result = new HashSet[Int]
//    for(si <- 0 until size) {
//      if (initialStateIndexProb(si) > 0.0) { result += si }
//    }
//    result.toSet
//  }

//  override def initialStates: Set[S] = initialStateIndices.map(state(_))
//
//  def finalStateIndices: Set[Int] = {
//    val result = new HashSet[Int]
//    for(si <- 0 until size) {
//      if (finalStateIndexProb(si) > 0.0) { result += si }
//    }
//    result.toSet
//  }

//  def initialStateProb(s: S): Double
//  = initialStateIndexProb(indexOf(s))
//
//  def finalStateProb(s: S): Double
//  = finalStateIndexProb(indexOf(s))
//
//  def transitionProb(s0: S, t: T, s1: S): Double
//  = {
//    val fromIdx:Int = indexOf(s0)
//    val labelIdx:Int = labelIndex(t)
//    val toIdx:Int = indexOf(s1)
//    transitionIndex(fromIdx, labelIdx, toIdx)
//  }
//
//  def eTransitionProb(s0: S, s1: S): Double = {
//    val fromIdx:Int = indexOf(s0)
//    val toIdx:Int = indexOf(s1)
//    eTransitionIndex(fromIdx, toIdx)
//  }
}
