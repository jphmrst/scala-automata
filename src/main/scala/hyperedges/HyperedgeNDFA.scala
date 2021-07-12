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
import org.maraist.graphviz.{NodeLabeling,TransitionLabeling}
import org.maraist.fa.elements.HasBuilder
import org.maraist.fa.NDFA
import org.maraist.fa.NDFA.IndexedNDFA
import org.maraist.fa.hyperedges.Builders.HyperedgeNDFAelements

/**
  *  @group Hyperedge
  */
trait HyperedgeNDFA[S, T, +ThisDFA <: IndexedHyperedgeDFA[Set[S], T]]
extends NDFA[S, T, ThisDFA] with Hyperedge[S] {
  /** {@inheritDoc} */
  override protected def internalsToDOT(
    stateList: IndexedSeq[S],
    sb: StringBuilder,
    nodeLabeling: NodeLabeling[S] = this.nodeLabeling,
    trLabeling: TransitionLabeling[T] = this.transitionLabeling):
      Unit = {
    super.internalsToDOT(stateList, sb, nodeLabeling, trLabeling)
    eHyperedgesToDOT(nodeLabeling, stateList, sb)
  }
}

/**
  *  @group Hyperedge
  */
trait IndexedHyperedgeNDFA[S,T,+ThisDFA <: IndexedHyperedgeDFA[Set[S],T]]
    extends HyperedgeNDFA[S,T,ThisDFA]
    with IndexedNDFA[S,T,ThisDFA]

/**
  *  @group Hyperedge
  */
object HyperedgeNDFA {
  def newBuilder[S, T](
    using impl: HasBuilder[
      HyperedgeNDFAelements,
      ?,
      [X,Y] =>> HyperedgeNDFA[X, Y, IndexedHyperedgeDFA[Set[X], Y]]
    ]
  ) = impl.build[S,T]()
}

