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
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle

/** Methods for builders of a deterministic finite automaton.
  *
  * @tparam S Type representing states.
  * @tparam T Type representing transition labels.
  * @tparam D Type of DFA constructed by this NFA.
  * @tparam G Collection type constructor for states in constructed
  * DFAs.
  * @tparam Z Type of style options for Graphviz export
  */
trait EdgeAnnotatedNFA[
  S, T, NA, DA,
  G[X] <: Set[X],
  +D[DS, DT, A] <: EdgeAnnotatedDFA[DS, DT, A, DZ],
  -NZ[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA],
  -DZ[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA]]

extends NFA[
  S, T, G,
  [DS, DT] =>> D[DS, DT, DA],
  [ZS, ZT] =>> NZ[ZS, ZT, NA],
  [ZS, ZT] =>> DZ[ZS, ZT, DA]]

with EdgeAnnotatedFA[S, T, NA, NZ]
