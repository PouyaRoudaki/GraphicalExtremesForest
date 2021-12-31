# graphicalExtremesForest
# Forest structure estimation for multivariate extremes 

Under the excellent supervision of Professor Engelke, I gained experience
conducting extensive research in Statistics in the areas of Extreme Value Theory, Conditional
Independence and Graphical Models, and Measure Theory. The idea is based on the fundamentals
provided in two published papers, and a contribution to the first paper.

1 Engelke, Hitz; 2020; Graphical models for extremes.

2 Engelke, Volgushev; 2020; Structure learning for extremal tree models.

3 Strokorb, 2020; Extremal independence old and new.

In the thesis, I am developing a new estimator to learn the underlying structure of disconnected
graphical models, mainly forests, on multivariate extremes coming from a Hüsler-Reiss
distribution. The new notion of conditional independence that has been used in this thesis comes
from (1) and (3). I extended the previous setting of Hüsler-Reiss distribution which is restricted to
connected graphs, to a new setting involving unconnected graphs. Specifically,

• I showed that the new theoretical setting agrees with the classical setting of HüslerReiss distribution on connected graphs.

• I provided an algorithm to estimate the underlying forest structure using the extremal
correlations as weights. The aforementioned algorithm is a modified version of Kruskal
algorithm. Using simulations and performance measurements, I illustrated that the
algorithm works efficiently.

• I proved the consistency of the estimator at the population level. I am currently
evaluating the asymptotic properties of the estimator for finite samples by simulation.

• I will also provide proofs of the estimator’s asymptotic performance and subsequently
use the estimator on a real data set.
