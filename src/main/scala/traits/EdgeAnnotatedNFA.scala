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
  +D[DS, DT] <: EdgeAnnotatedDFA[DS, DT, DA, Z],
  -Z[S, T] <: AutomatonStyle[S, T]]

extends EdgeAnnotatedFA[S, T, NA, Z]

with UnindexedEdgeAnnotatedNFA[S, T, NA, DA, G, D, Z]
