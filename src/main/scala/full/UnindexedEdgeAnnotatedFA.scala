// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.full
import org.maraist.fa.util.EdgeAnnotationCombiner
import org.maraist.fa.styles.{EdgeAnnotatedAutomatonStyle, DOT}
import org.maraist.fa.traits

/** Methods provided by an edge-annotated nondeterministic finite
  * automata (NFA).
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam A The type of annotations on transitions
  * @tparam K Type function/contructor producing the type of
  * annotation on DFAs from the type of annotation on NFAs.
  * @tparam D Type of DFA converted from this NFA
  *
  * @group Annotated
  */
trait UnindexedEdgeAnnotatedFA[
  S, T, A,
  -Z[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA]
]

extends traits.UnindexedEdgeAnnotatedFA[S, T, A, Z]

with UnindexedFA[S, T, [ZS, ZT] =>> Z[ZS, ZT, A]] {

  override def annotated(src: S, label: T, dest: S): Boolean =
    annotation(src, label, dest).isDefined

  override def eAnnotated(src: S, dest: S): Boolean =
    eAnnotation(src, dest).isDefined

  // =================================================================

  override protected def plotPresentEdge(
    sb: StringBuilder, style: Z[S, T, A],
    si0: Int, s0:S, ti0: Int, t:T, si1: Int, s1:S):
      Unit = {
    sb ++= DOT.tabToVmark
    sb ++= Integer.toString(si0)
    sb ++= DOT.graphvizArrowToVmark
    sb ++= Integer.toString(si1)
    sb ++= " [ label=<"
    sb ++= style.edgeLabel(t, s0, s1, this)
    annotation(s0, t, s1) match {
      case None => { }
      case Some(a) => {
        sb ++= "<br/>"
        sb ++= style.annotationLabel(a, t, s0, s1)
      }
    }
    sb ++= "> ];\n"
  }

  override protected def plotPresentEdge(
    sb: StringBuilder, style: Z[S, T, A],
    si0: Int, s0:S, si1: Int, s1:S):
      Unit = {
    sb ++= DOT.tabToVmark
    sb ++= Integer.toString(si0)
    sb ++= DOT.graphvizArrowToVmark
    sb ++= Integer.toString(si1)
    eAnnotation(s0, s1) match {
      case None => { }
      case Some(a) => {
        sb ++= " [ label=< &epsilon;<br/>"
        sb ++= style.eAnnotationLabel(a, s0, s1)
        sb ++= "> ]"
      }
    }
    sb ++= ";\n"
  }

  // =================================================================

  override protected def dumpTransition(
    src: S, label: T, dest: S, out: java.io.PrintStream):
      Unit = {
    out.println("- " + src)
    out.print("    -[ " + label + " ]->")
    annotation(src, label, dest) match {
      case None => { out.println(" (unann.)") }
      case Some(a) => { out.println(" : " + a) }
    }
    out.println("      " + dest)
  }

  override protected def dumpETransition(
    src: S, dest: S, out: java.io.PrintStream):
      Unit = {
    out.println("- " + src)
    out.print("    -->")
    eAnnotation(src, dest) match {
      case None => { out.println("  (unann.)") }
      case Some(a) => { out.println("  " + a) }
    }
    out.println("      " + dest)
  }
}
