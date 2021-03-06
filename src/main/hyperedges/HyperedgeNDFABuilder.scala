// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.hyperedges
import org.maraist.fa.{NDFA,NDFABuilder}

/**
  *  @group Hyperedge
  */
trait HyperedgeNDFABuilder[S,T, +ThisDFA <: IndexedHyperedgeDFA[Set[S],T],
                           +ThisNDFA <: NDFA[S,T,ThisDFA]]
extends NDFABuilder[S,T,ThisDFA,ThisNDFA, Builders.HyperedgeNDFAelements[S, T]]
      with HyperedgeNDFA[S,T,ThisDFA] with HyperedgeBuilder[S]
