
// -----------------------------------------------------------------
// DFA builder
// -----------------------------------------------------------------

abstract class AbstractHashEdgeAnnotatedDFABuilder
  [S, T, A,
    D <: AbstractEdgeAnnotatedArrayDFA[S,T,A],
    K >: Elements.AnnotatedDFAelement[S,T,A] <: Matchable
  ](initialState: S)
    extends AbstractHashDFABuilder[S,T,D,K](initialState)
    with DFAEdgeAnnotationsBuilder[S,T,A,D,K] {

  val edgeAnnotations: HashMap[S, HashMap[T, A]] =
    new HashMap[S, HashMap[T, A]]

  def annotation(src: S, label: T): Option[A] =
    edgeAnnotations.get(src) match {
      case None => None
      case Some(subhash) => subhash.get(label) match {
        case None => None
        case Some(ann) => Some(ann)
      }
    }

  def setAnnotation(src: S, label: T, annotation: A): Unit = {
    val subhash: HashMap[T, A] = edgeAnnotations.get(src) match {
      case Some(h) => h
      case None => {
        val h = new HashMap[T, A]
        edgeAnnotations(src) = h
        h
      }
    }
    subhash(label) = annotation
  }

  def removeAnnotation(src: S, label: T): Unit =
    edgeAnnotations.get(src) match {
      case Some(subhash) => {
        subhash.get(label) match {
          case Some(_) => {
            subhash -= label
            if subhash.size == 0
            then edgeAnnotations -= src
          }
          case None => { }
        }
      }
      case None => { }
    }

  protected final def assembleDFA(
    statesSeq: IndexedSeq[S],
    initialIdx: Int,
    finalStateIndices: HashSet[Int],
    transitionsSeq: IndexedSeq[T],
    idxLabels: Array[Array[Int]]
  ): D = {

    val edgeAnnotationsArray: Array[Array[Option[A]]] =
      Array.fill[Option[A]](statesSeq.length, transitionsSeq.length)(None)

    for (i <- 0 until statesSeq.length) {
      val src = statesSeq(i)
      val subhash = edgeAnnotations(src)

      for(j <- 0 until transitionsSeq.length) {
        val label = transitionsSeq(j)
        subhash.get(label) match {
          case Some(ann) => edgeAnnotationsArray(i)(j) = Some(ann)
          case None => { }
        }
      }
    }

    assembleDFA(
      statesSeq, initialIdx, finalStateIndices, transitionsSeq, idxLabels,
      edgeAnnotationsArray
    )
  }

  protected def assembleDFA(
    statesSeq: IndexedSeq[S],
    initialIdx: Int,
    finalStateIndices: HashSet[Int],
    transitionsSeq: IndexedSeq[T],
    idxLabels: Array[Array[Int]],
    edgeAnnotationsArray: Array[Array[Option[A]]]
  ): D


  /** Helper method for the [[scala.collection.mutable.Builder]]
    * implementation.
    */
  override protected def addBuilderElement(builder: K): Unit =
    builder match {
      case Elements.SetAnnotation(src, label, _, ann):
          Elements.SetAnnotation[S, T, A] => setAnnotation(src, label, ann)
      case Elements.RemoveAnnotation(src, label, _):
          Elements.RemoveAnnotation[S, T, A] => removeAnnotation(src, label)
      case e: DFAelements[S, T] => super.addBuilderElement(builder)
    }
}

class HashEdgeAnnotatedDFABuilder[S, T, A](initialState: S)
extends AbstractHashEdgeAnnotatedDFABuilder[
  S, T, A, EdgeAnnotatedArrayDFA[S, T, A],
  Elements.AnnotatedDFAelement[S,T,A]
](initialState) {

  protected def assembleDFA(
    statesSeq: IndexedSeq[S],
    initialIdx: Int,
    finalStateIndices: HashSet[Int],
    transitionsSeq: IndexedSeq[T],
    idxLabels: Array[Array[Int]],
    edgeAnnotationsArray: Array[Array[Option[A]]]):
      EdgeAnnotatedArrayDFA[S, T, A] =
    new EdgeAnnotatedArrayDFA[S,T,A](
      statesSeq,
      initialIdx,
      Set.from(finalStateIndices),
      transitionsSeq,
      idxLabels,
      edgeAnnotationsArray
    )

  type Traverser = DFAtraverser[S,T, ? >: this.type]
  protected def dotTraverser(sb: StringBuilder, stateList: IndexedSeq[S])(
    using style: GraphStyle[S, T]): Traverser =
    new org.maraist.fa.impl.DotTraverseDFA[S, T, this.type](style, sb, stateList, initialState)

}

