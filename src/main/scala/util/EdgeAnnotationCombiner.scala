// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.util
import scala.collection.mutable.{HashMap,HashSet,Queue}
import org.maraist.graphviz.{Graphable}
import org.maraist.fa.elements.NFAelements

/** Operations for converting NFA to DFA annotations, and for
  * combining pairs of annotations, when converting an annotated NFA
  * into an annotated DFA.
  *
  * @tparam A The type of annotations on transitions in the NFA
  * @tparam K Type function/contructor producing the type of annotation
  *
  * @group Annotated
  */
trait EdgeAnnotationCombiner[NA, DA] {

  /** Lift a single annotation to the result annotation type. */
  def single(a: NA): DA

  /** Incorporate another annotation. */
  def include(k: DA, a: NA): DA

  protected[fa] def updated(prev: Option[DA], ann: NA): DA = prev match {
    case None => single(ann)
    case Some(prevAnn) => include(prevAnn, ann)
  }

  /** Combine two converted annotations. */
  def combine(k1: DA, k2: DA): DA

  protected[fa] def combined(prev: Option[DA], curr: DA): DA =
    prev match {
      case None => curr
      case Some(prev) => combine(prev, curr)
    }
}

object EdgeAnnotationsCombiner {
  /** Default [[EdgeAnnotationCombiner]] using set union. */
  given singleSetCombiner[A]: EdgeAnnotationCombiner[A, Set[A]] with {
    def single(a: A): Set[A] = Set(a)
    def include(k: Set[A], a: A): Set[A] = k + a
    def combine(k1: Set[A], k2: Set[A]): Set[A] = k1 ++ k2
  }
}
