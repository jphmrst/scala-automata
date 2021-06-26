// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.hyperedges
import scala.collection.mutable.{Builder, HashMap, HashSet}
import java.awt.geom.GeneralPath
import org.maraist.fa.general.Builders.*
import org.maraist.fa.DFA.DFAelements
import org.maraist.fa.DFA.IndexedDFA
import org.maraist.fa.impl.{HashDFABuilder,HashNDFABuilder}
import org.maraist.fa.NDFA.NDFAelements
import org.maraist.fa.hyperedges.impl.
  {HashHyperedgeDFABuilder,HashHyperedgeNDFABuilder}

/**
  * @group General
  */
object Builders {

  case class AddEHyperedge[S](s: S, ss: Set[S])
  type EHyperedgeBuilders[S] = AddEHyperedge[S]

  type HyperedgeDFAelements[S, T]  = DFAelements[S, T]  | EHyperedgeBuilders[S]
  type HyperedgeNDFAelements[S, T] = NDFAelements[S, T] | EHyperedgeBuilders[S]

  given HasBuilderWithInit
    [HyperedgeDFAelements, HashHyperedgeDFABuilder, HyperedgeDFA]
      with {
    override def build[S,T](init: S): HashHyperedgeDFABuilder[S, T] =
      new HashHyperedgeDFABuilder[S, T](init)
  }

  given HasBuilder[
    HyperedgeNDFAelements,
    HashHyperedgeNDFABuilder,
    [X,Y] =>> HyperedgeNDFA[X, Y, IndexedHyperedgeDFA[Set[X], Y]]
  ] with {
    override def build[S,T](): HashHyperedgeNDFABuilder[S,T] =
        new HashHyperedgeNDFABuilder[S, T]
  }
}
