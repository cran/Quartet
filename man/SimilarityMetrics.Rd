% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Metrics.R
\encoding{UTF-8}
\name{SimilarityMetrics}
\alias{SimilarityMetrics}
\alias{DoNotConflict}
\alias{ExplicitlyAgree}
\alias{StrictJointAssertions}
\alias{SemiStrictJointAssertions}
\alias{SymmetricDifference}
\alias{RawSymmetricDifference}
\alias{RobinsonFoulds}
\alias{MarczewskiSteinhaus}
\alias{SteelPenny}
\alias{QuartetDivergence}
\alias{SimilarityToReference}
\title{Tree similarity measures}
\usage{
SimilarityMetrics(elementStatus, similarity = TRUE)

DoNotConflict(elementStatus, similarity = TRUE)

ExplicitlyAgree(elementStatus, similarity = TRUE)

StrictJointAssertions(elementStatus, similarity = TRUE)

SemiStrictJointAssertions(elementStatus, similarity = TRUE)

SymmetricDifference(elementStatus, similarity = TRUE)

RawSymmetricDifference(elementStatus, similarity = FALSE)

RobinsonFoulds(elementStatus, similarity = FALSE)

MarczewskiSteinhaus(elementStatus, similarity = TRUE)

SteelPenny(elementStatus, similarity = TRUE)

QuartetDivergence(elementStatus, similarity = TRUE)

SimilarityToReference(elementStatus, similarity = TRUE, normalize = FALSE)
}
\arguments{
\item{elementStatus}{Two-dimensional integer array, with rows corresponding to
counts of matching quartets or partitions for each tree, and columns named
according to the output of \code{\link[=QuartetStatus]{QuartetStatus()}} or
\code{\link[=SplitStatus]{SplitStatus()}}.}

\item{similarity}{Logical specifying whether to calculate the similarity
or dissimilarity.}

\item{normalize}{Logical; if \code{TRUE}, a random or star tree has expected
similarity 0 (or difference 1), and the maximum possible score is one.
If \code{FALSE}, zero similarity corresponds to all quartets contradicted,
whereas one corresponds to all quartets correctly resolved -- which will be
unattainable if the reference tree contains polytomies.}
}
\value{
\code{SimilarityMetrics()} returns a named two-dimensional array in which each row
corresponds to an input tree, and each column corresponds to one of the
listed measures.

\code{DoNotConflict()} and others return a named vector describing the requested
similarity (or difference) between the trees.
}
\description{
Measure tree similarity or difference.
}
\details{
\insertCite{Estabrook1985;textual}{Quartet} (table 2) define four similarity
metrics in terms  of the total number of quartets (\emph{N}, their \emph{Q}),
the number of quartets resolved in the same manner in two trees (\emph{s}),
the number resolved differently in both trees (\emph{d}),
the number resolved in tree 1 or 2 but unresolved in the other tree
(\emph{r1}, \emph{r2}), and the number that are unresolved in both trees (\emph{u}).

The similarity metrics are then given as below.  The dissimilarity metrics
are their complement (i.e. 1 - \emph{similarity}), and can be calculated
algebraically using the identity \emph{N} = \emph{s} + \emph{d} + \emph{r1} + \emph{r2} + \emph{u}.

Although defined using quartets, analogous values can be calculated using
partitions -- though for a number of reasons, quartets may offer a more
meaningful measure of the amount of information shared by two trees
\insertCite{Smith2020}{Quartet}.
\itemize{
\item Do Not Conflict (DC): (\emph{s} + \emph{r1} + \emph{r2} + \emph{u}) / \emph{N}
\item Explicitly Agree (EA): \emph{s} / \emph{N}
\item Strict Joint Assertions (SJA): \emph{s} / (\emph{s} + \emph{d})
\item SemiStrict Joint Assertions (SSJA): \emph{s} / (\emph{s} + \emph{d} + \emph{u})
}

(The numerator of the SemiStrict Joint Assertions similarity metric is
given in \insertCite{Estabrook1985;textual}{Quartet} table 2 as \emph{s} + \emph{d},
but this is understood, with reference to their text, to be a typographic
error.)

\insertCite{Steel1993;textual}{Quartet} propose a further metric,
which they denote d_Q_,
which this package calculates using the function \code{SteelPenny()}:
\itemize{
\item Steel & Penny's quartet metric (dQ): (\emph{s} + \emph{u}) / \emph{N}
}

Another take on tree similarity is to consider the symmetric difference:
that is, the number of partitions or quartets present in one tree that do
not appear in the other, originally used to measure tree similarity by
\insertCite{Robinson1981;textual}{Quartet}.
(Note that, given the familiarity of the Robinson–Foulds distance metric,
this quantity is be default expressed as a difference rather than a
similarity.)
\itemize{
\item Raw symmetric difference (RF): \emph{d1} + \emph{d2} + \emph{r1} + \emph{r2}
}

A pair of trees will have a high symmetric difference if they are
well-resolved but disagree on many relationships; or if they agree on
most relationships but are poorly resolved.
As such, it is essential to contextualize the symmetric difference by
appropriate normalization \insertCite{Smith2019}{Quartet}.
Multiple approaches to normalization have been proposed:

The total number of resolved quartets or partitions present in both trees
\insertCite{Day1986}{Quartet}:
\itemize{
\item Symmetric Difference (SD): (2 \emph{d} + \emph{r1} + \emph{r2}) / (2 \emph{d} + 2 \emph{s} + \emph{r1} + \emph{r2})
}

The total distinctly resolved quartets or partitions
\insertCite{Marczewski1958,Day1986}{Quartet}:
\itemize{
\item Marczewski-Steinhaus (MS): (2 \emph{d} + \emph{r1} + \emph{r2}) / (2 \emph{d} + \emph{s} + \emph{r1} + \emph{r2})
}

The maximum number of quartets or partitions that could have been resolved,
given the number of tips \insertCite{Smith2019}{Quartet}:
\itemize{
\item Symmetric Divergence: (\emph{d} + \emph{d} + \emph{r1} + \emph{r2}) / \emph{N}
}

Finally, in cases where a reconstructed tree \code{r1} is being compared to a
reference tree \code{r2} taken to represent "true" relationships,
a symmetric difference is not desired.
In such settings, the desired score is the expectation that a given
quartet's resolution in the reconstructed tree is "correct", given by
\insertCite{Asher2020;textual}{TreeTools}:
\itemize{
\item Similarity to Reference (S2R): (\emph{s} + (\emph{r1} + \emph{r2} + \emph{u}) / 3) / \emph{Q}
}

This may optionally be normalized with reference to the maximum possible
similarity, (\emph{s} + \emph{d} + \emph{r2} + (\emph{r1} + \emph{u}) / 3) / \emph{Q}, subtracting
1/3 (the probability of matching at random) from both the S2R score and
maximum possible score before dividing; then, a tree scores zero if it
is as different from the true tree as a random or fully unresolved tree,
and one if it is as "true" as can be known.
}
\examples{
data("sq_trees")

sq_status <- QuartetStatus(sq_trees)
SimilarityMetrics(sq_status)
QuartetDivergence(sq_status, similarity = FALSE)

library("TreeTools", quietly = TRUE, warn.conflict = FALSE)
set.seed(0)
reference <- CollapseNode(as.phylo(101, 10), 16:18)
trees <- c(
  reference = reference,
  binaryRef = MakeTreeBinary(reference),
  balanced = BalancedTree(reference),
  pectinate = PectinateTree(reference),
  star = StarTree(reference),
  random = RandomTree(reference),
  random2 = RandomTree(reference)
)
elementStatus <- QuartetStatus(trees, reference)
SimilarityToReference(elementStatus)
SimilarityToReference(elementStatus, normalize = TRUE)
}
\references{
\insertAllCited{}
}
\seealso{
\itemize{
\item Calculate status of each quartet -- the raw material from which the
Estabrook \emph{et al.} metrics are calculated -- with \code{\link[=QuartetStatus]{QuartetStatus()}}:
\item Equivalent metrics for bipartition splits: \code{\link[=SplitStatus]{SplitStatus()}},
\code{\link[=CompareSplits]{CompareSplits()}}
}
}
\author{
\href{https://orcid.org/0000-0001-5660-1727}{Martin R. Smith}
(\href{mailto:martin.smith@durham.ac.uk}{martin.smith@durham.ac.uk})
}
