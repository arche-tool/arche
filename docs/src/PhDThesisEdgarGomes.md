 

![image](frontmatter/logos/UGent){height="0.5in"}![image](frontmatter/logos/DMSE){height="0.5in"}

Literature Review
=================

Background
----------

Introduction
------------

Computational tools
-------------------

### Introduction

### Program languages

### Development tools

### Stereographic visualization

VirMat
======

Generation of grains
--------------------

### Introduction

### Morphological aspects

### Volume representation

### Voronoi cell

### Sphere packing

### Statistical analysis

### Topological analysis

Grain boundary surfaces
-----------------------

### Introduction

### Surface representation

There many possible ways to represent finite surfaces with arbitrary
topology. Some of the most common methods will be describe and their
main advantages/disadvantages will be highlighted.

##### Polygon

The most simple and straightforward surface representation is the
polygon. A grain boundary can be represented by simple (non
self-intersecting) polygon where its vertexes, edges and internal area
correspond, respectively, to quadruple junctions, triple lines and the
grain boundary. Polygons are limited to represent flat grain boundaries
and straight triple lines.

##### Voxel

##### Marching cubes

##### NURBS

### Subdivision surfaces

### Subdivision surface as grain boundary

Orientation distribution {#ch:Orientation}
------------------------

Douglas Hofstadter - Gödel, Escher, Bach: An Eternal Golden Braid - 1999

ome quick intro here\...

### Introduction

### Crystallographic Orientation

Crystallographic orientations, in polycrystalline materials, are
represented by a rotation between the crystal lattice and the chosen ERS
that is macroscopically fixed to the material. The ERS is normally
defined according to the conformation process used to obtain the
polycrystalline material. For example, when processed by rolling mills,
the RD (direction along the plate displacement), the ND (direction
normal to the plate's surface) and the TD (direction along the axis of
the rolls) are used as a reference system. The ERS may change according
to the analysis that has to be done or the processing steps involved.
For instance, spiral pipes use rolled plates with an angular rotation
from the pipe's axis, and therefore, a new ERS may be defined with the
pipe's axial direction as X-axis, instead.

$$K_{C}=gK_{ERS}\label{eq:ori_def}$$

The crystallographic orientation $g$ is defined as the rotation applied
to the ERS ($K_{ERS}$) that brings into coincide with the crystal
lattice reference system ($K_{C}$), see equation
[\[eq:ori\_def\]](#eq:ori_def){reference-type="ref"
reference="eq:ori_def"}. This can also be interpreted as a passive
transformation where the basis forming the ERS is transformed into a new
basis corresponding to the crystal reference system. The active
rotation, on the contrary, rotates a vector and the resulting
transformation is still represented on the original basis. Figure
[\[fig:diff-act-pas-rot\]](#fig:diff-act-pas-rot){reference-type="ref"
reference="fig:diff-act-pas-rot"} shows the difference between active
and passive rotation. The active rotation applies the transformation
directly to a vector $v_{A}$ represented in the basis $A$, resulting in
a vector $v'_{A}$ that is still represented in the basis $A$.
Conversely, the passive rotation applies the transformation to the basis
$A$ and the result is the rotated basis $B$. In this case, if the vector
$v'$ is applied in the basis $B$ then the relation $v_{A}=v'_{B}$ is
valid. Note that the rotations, on both transformations, have the same
rotation axis but opposite rotation angles.

Rotations, and therefore orientations, are fixed point transformations
i.e. they are a linear transformation where at least one point remains
invariant, so if $g:\mathbb{R}^{n}\rightarrow\mathbb{R}^{n}$ is a
rotation and $u\in\mathbb{R}^{n}$ then $u$ is called fixed point or axis
of rotation if and only if $g\left(u\right)=u$. Rotations also form a
mathematical group called *rotation group* that is a non-abelian group
under composition. Let $G$ be the set of linear transformations
$g:\mathbb{R}^{n}\rightarrow\mathbb{R}^{n}$ forming the rotation group
and $\left(\bullet\right)$ be the binary operator, then the following
properties are valid (a graphic illustration of the properties is shown
on Figure [\[fig:rot-group\]](#fig:rot-group){reference-type="ref"
reference="fig:rot-group"}):

-   *Closure:* If $g_{1}$and $g_{2}$ are two arbitrary rotation
    ($g_{1}\in G$ and $g_{2}\in G$) then there is a functional
    composition operator $\left(\bullet\right):G\times G\rightarrow G$
    which implies that $g_{1}\bullet g_{2}\in G$. It means that the
    combination of any rotations is always a valid rotation.

-   *Associativity:* For $\left\{ g_{1},g_{2},g_{3}\right\} \in G$ then
    $g_{1}\bullet\left(g_{2}\bullet g_{3}\right)=\left(g_{1}\bullet g_{2}\right)\bullet g_{3}$

-   *Identity element:* There an identity element $id$ that composed
    with any $g\in G$ changes nothing, so the equation
    $id\bullet g=g\bullet id=g$ is valid.

-   *Inverse element:* For any $g\in G$, there is an element $g^{-1}$
    such that the equation $g\bullet g^{-1}=g^{-1}\bullet g=id$ holds
    valid.

-   *Non-commutativity:* If $g_{1}$ and $g_{2}$ are two arbitrary
    rotation then the equation $g_{1}\bullet g_{2}=g_{1}\bullet g_{2}$
    may *not* be valid.

### Symmetry

$$\begin{alignedat}{1}\left(O_{m}g_{a}\right)\left(O_{n}g_{b}\right)^{-1}=I\\
O_{m}g_{a}O_{n}=g_{b}\\
\left(O_{m}g_{a}\right)O_{n}=g_{b}
\end{alignedat}$$

### Orientation representation

There many ways to represent physical rotations ($\mathbb{R}^{3}$). The
most important ones are described bellow, as well as their
advantages/disadvantages and their conversion to quaternions.

#### Matrix representation

Rotation matrix is a square matrix ($n\times n$) with real values
entries ($a_{ij}\in\mathbb{R}^{n}$) used to perform rotation in the
Euclidean space. Moreover, rotation matrices are orthogonal matrices
with determinate equal $1$. These two features can be used as a
validation test, for instance, if $R$ is a real square matrix then $R$
can be a valid rotation matrix only if,

$$RR^{T}=I,\det R=1.$$

Rotation matrices form a group when using matrix multiplication as
function composition. In this case, the group properties are represented
as the following:

-   *Closure:* If $R_{1}$and $R_{2}$ are two arbitrary rotation matrices
    ($R_{1}\in\mathbb{M}\left(n,n\right)$ and
    $R_{2}\in\mathbb{M}\left(n,n\right)$) and matrix multiplication is
    the functional composition then $R_{1}R_{2}=R_{3}$ and
    $R_{3}\in\mathbb{M}\left(n,n\right)$. Where $R_{3}$ is the rotation
    $R_{2}$ followed by the rotation $R_{1}$.

-   *Associativity:* For
    $\left\{ R_{1},R_{2},R_{3}\right\} \in\mathbb{M}\left(n,n\right)$
    then $R_{1}\left(R_{2}R_{3}\right)=\left(R_{1}R_{2}\right)R_{3}$

-   *Identity element:* $id=I$ is the identity matrix (when all the
    elements are $0$ but the elements of the diagonal which are $1$).
    Therefore $IR=RI=R$ is valid.

-   *Inverse element:* Due the orthogonality of rotation matrices, the
    relation $RR^{T}=I=RR^{-1}$ is valid. Therefore the inverse rotation
    is the transpose matrix, $R^{-1}=R^{T}$.

Simple rotation around the Cartesian ($\mathbb{R}^{3}$) axes are given
as an example in the equation
[\[eq:simpleRotMat\]](#eq:simpleRotMat){reference-type="ref"
reference="eq:simpleRotMat"}, where $\phi$ is the angle of rotation.

$$\begin{alignedat}{1}R_{x}\left(\phi\right)=\left[\begin{array}{ccc}
1 & 0 & 0\\
0 & \cos\phi & \sin\phi\\
0 & -\sin\phi & \cos\phi
\end{array}\right]\\
R_{y}\left(\phi\right)=\left[\begin{array}{ccc}
\cos\phi & 0 & -\sin\phi\\
0 & 1 & 0\\
\sin\phi & 0 & \cos\phi
\end{array}\right]\\
R_{z}\left(\phi\right)=\left[\begin{array}{ccc}
\cos\phi & \sin\phi & 0\\
-\sin\phi & \cos\phi & 0\\
0 & 0 & 1
\end{array}\right]
\end{alignedat}
\label{eq:simpleRotMat}$$

When rotation matrices are used to represent orientations, its rows and
columns have special meaning. The rows correspond to the Cartesian
directions of the crystal basis represented in ERS whereas the columns
correspond to the Cartesian directions of the ERS represented in the
crystal basis. For instance, the first row is the direction of the
crystallographic $x$ axis in macroscopic frame and the last row is the
normal direction to the surface's plate observed from crystallographic
perspective, as indicated in Figure
[\[fig:ori\_mat\_info\]](#fig:ori_mat_info){reference-type="ref"
reference="fig:ori_mat_info"}.

The second and last column (RD and ND in Figure
[\[fig:ori\_mat\_info\]](#fig:ori_mat_info){reference-type="ref"
reference="fig:ori_mat_info"}) can also form a simplified orientation
representation called Miller indices. Miller indices are composed of two
directions: $\left(hkl\right)$ the crystallographic direction of the
plane parallel to the surface's plate i.e.
$\left(hkl\right)\parallel ND$, and $\left[uvw\right]$ the
crystallographic direction contained in $\left(hkl\right)$ plane and
parallel to the rolling direction i.e. $\left[uvw\right]\parallel RD$
and $\left[uvw\right]\mathrel\bot RD$. The remaining column (TD) is
easily calculated from the cross product between $\left(hkl\right)$ and
$\left[uvw\right]$. Although there is an identity element for Miller
indices ($id$=$\left(001\right)\left[100\right]$), no composition
function nor inversion function exist. Therefore, Miller indices do not
form a proper orientation representation.

Although rotation matrices are a simple orientation representation due
its simplified operations for composition, identity and inversion, there
some drawbacks. In the case of rotation in $\mathbb{R}^{3}$, there are 9
elements to store for a 3 degrees freedom transformation, which means
that 6 elements are implicitly redundant. Moreover, the space of
rotation is a 9-dimension space which is practically impossible to
visualize. And the rotation composition requires 27 multiplications and
18 additions.

#### Euler angles

Euler angles represent arbitrary rotations by composing a sequence of
three rotations around three distinct axes. Different conventions or
sequences of rotation exist with the Bunge convention being the most
common in the field of crystallographic texture analysis. Bunge
convention corresponds to the $ZXZ$ sequence where the first rotation is
around the $Z$ axis with $\varphi_{1}$ angle followed with a rotation
around $X'$ ($X$ axis after the first rotation) with $\Phi$ angle and
the last rotation is on $Z''$ with $\varphi_{2}$ angle. This sequence
can be visualized in figure
[\[fig:EulerZXZ\]](#fig:EulerZXZ){reference-type="ref"
reference="fig:EulerZXZ"}. The Euler angles can represented in the
matrix from using the rotations in the equation
[\[eq:simpleRotMat\]](#eq:simpleRotMat){reference-type="ref"
reference="eq:simpleRotMat"}:

$$E\left(\varphi_{1},\Phi,\varphi_{2}\right)=R_{z}\left(\varphi_{2}\right)R_{x}\left(\Phi\right)R_{z}\left(\varphi_{1}\right)\label{eq:eulerMatrix}$$

By splitting the rotation around the main axes, the Euler angles allow a
more compact representation than matrices using only three angles that
is the minimum number of parameters necessary to represent a solid
rotation with three degrees of freedom.

Although widely used in some scientific communities, the Euler angles do
not have well know and clear composition and inverse functions. And the
most critical drawback is the presence of singularities, it means that
there are some rotation with more than one representation in the Euler
space. For instance, the rotation $\left(45,0,0\right)$ is exactly the
same as the rotation $\left(0,0,45\right)$ or $\left(20,0,25\right)$.

#### Axis-Angle

Axis-Angle rotation is represented by a normalized direction $\vec{v}$
and a rotation angle $\theta$ around $\vec{v}$ following the right-hand
grip rule. Like the Euler representation, it has a compact
representation $\mathbb{SO2\times\mathbb{R}}$ but it has trivial inverse
function. The use of an unique rotation axis in combination with an
rotation angle makes this representation very human readable, specially
for representing difference between rotations (i.e. misorientations at
the grain boundaries). One should be aware the singularity that exists
when $\theta=0$, in this case the representation of the normalized
direction is meaningless and any arbitrary$\vec{v}$ will represent the
identity rotation.

The inverse rotation can be easily obtained by either reversing the
direction vector or by negating the rotation angle. This also implies
that some constraint has to be applied to the domain boundaries of the
axis-angle space in order to avoid multiple equivalent representations.
For instance,
$\left(\vec{v},\theta\right)=\left(\vec{-v},2\pi-\theta\right)$ if
$0\leq\theta<2\pi$. The solution is restrict $\theta$ in the range
$0\leq\theta<\pi$ or $-\nicefrac{\pi}{2}\leq\theta<\nicefrac{\pi}{2}$ or
even, although less desirable, restrict the $\vec{v}$ to be contained in
half $\mathbb{SO2}$.

#### Frank-Rodriges

The Frank-Rodriges has, as Euler angles, the most possible compact
representation of rotations.

#### Quaternion

### Orientation distribution {#orientation-distribution}

#### Bingham distribution

##### Normalization constant

The *generalized hypergeometric function*[]{lang="en-US"}[^1] is defined
as

$$_{p}F_{q}\left(a_{1},\ldots,a_{p};b_{1},\ldots,b_{q};z\right)=\sum_{j=0}^{\infty}{\textstyle \frac{\left(a_{1}\right)_{j}\ldots\left(a_{p}\right)_{j}}{\left(b_{1}\right)_{j}\ldots\left(b_{q}\right)_{j}}\frac{z^{j}}{j!}}\label{eq:generalized_hypergeometric}$$

Where $a_{1},\ldots,a_{p},b_{1},\ldots,b_{q},z\mathbb{\in C}$ and the
*Pochhammer symbol* $\left(x\right)_{n}$represents the rising factorial
as the following

$$(x)_{n}=x(x+1)(x+2)\cdots(x+n-1)$$

The *confluent hypergeometric function*, also known as *Kummer's
function*, is a specific case of the *generalized hypergeometric
function* where $p=q=1$, therefore the
[\[generalized\_hypergeometric\]](#generalized_hypergeometric){reference-type="eqref"
reference="generalized_hypergeometric"} becomes

$$_{1}F_{1}=\sum_{j=0}^{\infty}{\textstyle \frac{\left(a\right)_{j}}{\left(b\right)_{j}}\frac{z^{j}}{j!}}$$

Factorials are time consuming to calculate but no precomputed table can
be used in this case because *Pochhammer symbol* involves real numbers.
Alternatively, *Pochhammer symbol* for rising factorials can be
calculated using the gamma function $\Gamma$

$$(x)_{n}=\frac{\Gamma(x+n)}{\Gamma(x)}$$

##### Multivariate confluent hypergeometric function

The multi-dimension confluent hypergeometric function is given by one of
the Appell series functions

$$F_{1}(0,a_{1},\ldots,a_{n},b,x_{1},\ldots,x_{n})=\sum_{k_{1},\ldots,k_{n}}{\textstyle \frac{\left(a_{1}\right)_{k_{1}}\ldots\left(a_{n}\right)_{k_{n}}}{\left(b\right)_{k_{1}+\ldots+k_{n}}}\frac{x_{1}^{j}\ldots x_{n}^{j}}{k_{1}!\ldots k_{n}!}}$$

And its partial derivatives regarding $x$ are given by

$$\frac{\partial}{\partial x}F_{1}(a,b_{1},b_{2},c;x,y)=\frac{ab_{1}}{c}F_{1}(a+1,b_{1}+1,b_{2},c+1;x,y)$$

#### Hyper Spherical Harmonics

![Active and passive rotations on the quaternion space using HSH. The
original orientations are:
$s_{1}=SO^{3}\:(\nicefrac{\pi}{2},\nicefrac{\pi}{2},\nicefrac{\pi}{2})$
and $s_{2}=SO^{3}\:(\nicefrac{\pi}{4},\nicefrac{\pi}{4},0)$. Both active
and passive rotation are
$SO^{3}\:(\nicefrac{-\pi}{2},\nicefrac{\pi}{2},0)$.](\string"mainmatter/img/oridist/HSH active and passive rotations\string".png){width="100%"}

![Enforcing symmetry on
HSH.](\string"mainmatter/img/oridist/HSH symmetryzation\string".png){width="100%"}

Texture Sampling
----------------

### Introduction {#introduction}

Polycrystalline metallic solids are composed of a contiguous set of
crystal grains. Such a set and its properties constitute the
microstructure and can be partially described using statistical
parameters like grain size, crystallographic orientation and
misorientation distributions. To a large extent the microstructure of a
polycrystal determines its engineering behavior by controlling *e.g.*
mechanical, corrosion and magnetic properties. In the last decades a
large number of models has been developed claiming to account for a
variety of material responses and some of these may benefit from
calculations employing 3D microstructures as an input.

During the development of a microstructurally based material model a set
of different input parameters are required for validation and
optimization. In order to avoid time-consuming and sophisticated 3D
measurements of microstructures a framework for virtual microstructure
generation is proposed here. A virtual microstructure is a model itself,
capable of generating a numerical representation of numerous
microstructural features that are able to substitute for experimental
data when used as input to microstructural models. Virtual
microstructures are very handy to produce test case samples, which can
be used for fast modeling prototyping and microstructural design since
they may cover a very large spectrum of microstructural state variables
even outside the range of practical implementation. Such capacity helps
one to find the boundary conditions and the crucial state variables in a
model. It is important to state that the aim is not to completely
replace experiments but reduce their number and improve their efficiency
by predefining which samples are better suited for experimental
validation. Once validated and with the boundary conditions checked, it
is also useful to look for optimum microstructures when one wants to
design a material.

In the past decades metallurgists and scientists have been looking at
the microstructure and defined a broad range of parameters to
characterize them. However to create a virtual microstructure the
opposite exercise has to be done, *i.e.* starting from a set of
microstructural state variables, conveniently expressed as statistical
distribution functions, a 3D set of contiguous grains needs to be
created which represents the virtual microstructure. Because
microstructures are very complex objects some assumptions are required
to simplify the framework. Therefore, it is assumed here that the
microstructure is fully characterized by a set of crystal orientations
of a single phase that are assembled in a contiguous volume, ignoring
in-grain heterogeneities. The input parameters of the microstructure
generator can be specified as a set of distribution functions and their
relations like grain morphology, orientation and grain boundary
character. Additional features such as number of neighbors, triple
junction angles or clustering of some properties might be considered as
well, but are ignored for the time being. The current paper will report
the results when the grain size and texture distribution functions are
given as input.

### Method

In the proposed framework, surfaces are the main representative objects.
Each grain is represented by a set of surfaces forming a closed volume
rather than a set of subvolumes in a grid as *e.g.* voxels in a cubic
grid space. This choice allows for a more precise description of grain
boundaries and associated properties (such as *e.g.* local GB
curvature), and on the practical side it also allows for a compact data
description and thus a faster calculation. In the first version of the
model the boundaries are composed of flat surfaces, but in future these
flat surfaces will be converted to polynomial piecewise surfaces. If
necessary, further post-processing conversion to voxels or meshes can be
carried out for the sake of compatibility with others model formats such
as *e.g.* finite elements and phase field modeling.

The grains are first created using the concept of Voronoi cells, which
consist of a mathematical concept where a space $\mathbb{R}^{n}$ is
partitioned starting from a set of points $p\in\mathbb{R}^{n}$. Each
cell is defined by a region in the space where all distances are closer
to its central point $p$ than the central points from any other cell.
Voronoi structures bear a one-to-one relation with Delaunay
triangulation  [@aurenhammer1991voronoi]. Both concepts provide
equivalent information but stored in different ways, the so called
duality property, that allows a straightforward bijective conversion
between them. Even though there are algorithms to directly calculate
Voronoi cells, an indirect method that calculates the Delaunay
triangulation first and then converts it to Voronoi cells is used here.

The key point to generate a proper grain size distribution using Voronoi
cells is to find a proper set of central points that reproduces the user
specified grain size distribution. The user provides the grain size
distribution function $f(gs)$ as well as the lower and upper limits
$gs\in[a,b]$, thus defining the *target* distribution. If $n$ points are
randomly placed in a box of which the size depends on the number of
grains $n$ and the average grain size of the target distribution, the
resultant distribution is the so-called Poisson-Voronoi which exhibits a
lognormal distribution with a variance of $0.424$ [@xu2009topological].
Even though this distribution might be useful in some specific cases, in
more general cases the target distribution will deviate from the
lognormal one and may exhibit a different variance or even a bimodal
structure. In order to transform the initial set of points in a valid
one where the Voronoi cells follow the target distribution a RMC
algorithm is used  [@mcgreevy2001reverse]. Both target and current
distributions are discretized into histograms with volume fractions
$p_{i}$ in each bin and the error function described in equation
[\[eq:gs\_err\]](#eq:gs_err){reference-type="ref" reference="eq:gs_err"}
is employed for convergence.

The orientation assignment starts once a suitable grain size
distribution is found. The target distribution is obtained from a
discretized ODF in Euler space. To the purpose of discretization the
inverse of the CDF is sampled by a uniform distribution, very much
similar to the method proposed by Toth and Van Houtte
 [@toth1992discretization]. The CDF is constructed by numerical
integration of the ODF along its three angles, as describe in equation
[\[eq:cdf\]](#eq:cdf){reference-type="ref" reference="eq:cdf"} where
$v_{i,j,k}$ is the volume fraction in a discrete position of the Euler
space.

$$\begin{aligned}
error_{GS}=\sum_{i=1}^{N}(p_{i}-p_{i}^{target})^{2}.\label{eq:gs_err}\\
error_{ODF}=\sum_{i=1}^{N}\sum_{j=1}^{M}\sum_{k=1}^{L}(v_{i,j,k}-v_{i,j,k}^{target})^{2}.\label{eq:odf_err}\\
CDF(N,M,L)=\sum_{i=1}^{N}\sum_{j=1}^{M}\sum_{k=1}^{L}v_{i,j,k}\sin\Phi\Delta\phi_{1}\Delta\Phi\Delta\phi_{2}.\label{eq:cdf}\end{aligned}$$

Even though the method described above allows a fair ODF sampling , each
sampled orientation has implicitly the same weight or volume fraction.
It means that when the orientations are associated to the grains and the
grain set has a non-uniform volume fraction distribution, the resultant
ODF is very much likely to be distorted. On the other hand there are an
infinite set of orientations that when associated with a certain set of
grains will result in equivalent ODFs. This shows how ill-posed the
problem might be *i.e.* in its present form it is not guaranteed that a
unique solution can be found. In order to specify a unique solution the
missing state variables like misorientation or grain boundary character
distributions should be included. For the moment the model does not
include any of those parameters, therefore RMC will be applied to
enforce a *possible* association between orientations and volume
fractions using the error function described in equation
[\[eq:odf\_err\]](#eq:odf_err){reference-type="ref"
reference="eq:odf_err"}.

### Example

One example displaying the current capabilities of the virtual
microstructure generator will be given. A set of 1000 grains was
generated and fitted into a bimodal spectrum composed by overlapping two
normal distributions. The first distribution, that was obtained by
placing points randomly, is shown in figure
[\[fig:odf\_dist\_target\]](#fig:odf_dist_target){reference-type="ref"
reference="fig:odf_dist_target"}(a). As expected, it resembles a
lognormal distribution with a variance of approximately $0.43$ that is
typical for Poisson-Voronoi statistics. Finally, after the RMC
iterations, the desired distribution is obtained as one can observe in
figure
[\[fig:odf\_dist\_target\]](#fig:odf_dist_target){reference-type="ref"
reference="fig:odf_dist_target"}(b).

![[\[fig:odf\_dist\_target\]]{#fig:odf_dist_target
label="fig:odf_dist_target"}Generation of 1000 grains that correspond to
the target distribution composed by two overlapping normal distributions
($\mu_{1}=1,\;\sigma_{1}=0.5,\;\mu_{2}=5,\;\sigma_{2}=0.7$). (a) The
initial distribution after random positioning of points. (b) Final
distribution after RMC iterations. (c) 3D section of the resultant grain
set. ](mainmatter/img/texsamp/fig1){width="100%"}

Once the grain set is defined, the following step is to create the
texture by assigning orientations to each grain. Given a target ODF,
orientations were sampled and assigned randomly to the grains, see
figure
[\[fig:recons\_odf\_brutal\]](#fig:recons_odf_brutal){reference-type="ref"
reference="fig:recons_odf_brutal"}(b). As expected, the distribution
does not match with the target distribution, figure
[\[fig:recons\_odf\_brutal\]](#fig:recons_odf_brutal){reference-type="ref"
reference="fig:recons_odf_brutal"}(a), because of the non-uniform grain
size. After the RMC iterations a valid configuration was achieved as
shown on figure
[\[fig:recons\_odf\_brutal\]](#fig:recons_odf_brutal){reference-type="ref"
reference="fig:recons_odf_brutal"}(c).

![[\[fig:recons\_odf\_brutal\]]{#fig:recons_odf_brutal
label="fig:recons_odf_brutal"}Texture reconstruction. (a) Target ODF
with typical $\alpha-\gamma$ fibers obtained experimentally. (b) Initial
ODF after random assignment of sampled orientations to grains. (c) Final
ODF after RMC iterations. (d) Overlay of the previous three
states.](mainmatter/img/texsamp/Fig2){width="100%"}

### Sampling distribution

Sampling is generation of a set of values that follows determined
statistical descriptors. Therefore, sampling is inverse of the
statistical analysis which derive statistical descriptors for a given
set of values, also known as population. If the generated population is
large enough, both initial statistical descriptors and statistical
descriptors derived from the analysis of the generated population should
be fairly close. For example, in figure
[\[fig:SamplingNormal1D\]](#fig:SamplingNormal1D){reference-type="ref"
reference="fig:SamplingNormal1D"} a normal distribution in sampled and
the resultant points form a Gaussian distribution close to the original
distribution.

The example above is very simple and the sampled points can be obtained
even with an analytic equation from cumulative distribution function of
the normal distribution. But Nature is not always so kind and the
distributions derived from most of features in polycrystal, like grain
size distribution, may have complex shape with more than one mode (peak
with higher density of values). And in the specific case of
crystallographic orientation distributions, it is even more complex due
its higher dimension, symmetry and non-linearity of the orientation
space.

There are many techniques for sampling multidimensional distributions
but the majority are defined for Euclidean space. Some of them and a new
one adapted for orientation space will be described in the following
sections.

#### Inverse Transform Sampling

This is a basic method for generating random samples from an arbitrary
probability distribution function. Let $p\left(x\right)$ be a normalized
probability distribution function defined over the interval
$\left[a,b\right]$, the method begins with the calculation of the
cumulative distribution function:

$$F\left(x\right)=\int_{a}^{x}p\left(t\right)dt$$

Since $p\left(x\right)$ is normalized, $F\left(a\right)=0$ and
$F\left(b\right)=1$. The next step is to obtain the inverse of the
cumulative distribution function, $F^{-1}\left(u\right)$ where:

$$\begin{aligned}\left\{ u\in\mathbb{R}\mid0\leq u\leq1\right\} \\
u=F\left(F^{-1}\left(u\right)\right)\\
a\leq F^{-1}\left(u\right)\leq b
\end{aligned}$$

Random values following $p\left(x\right)$ can be obtained by feeding the
inverse of the cumulative distribution function with values from an
*uniform* random distribution in the interval $\left[0,1\right]$.
Uniform random distribution is a probability distribution function with
constant probability in its interval. This methodology is illustrated in
figure [\[fig:InvSampling1D\]](#fig:InvSampling1D){reference-type="ref"
reference="fig:InvSampling1D"}.

This technique is quite general and it works with any continuous or
discrete probability functions, i.e.
$\left\{ p\left(x\right)\geq0\mid a\leq x\leq b\right\}$. If the
cumulative function or its inverse do not have an analytic description
or if them are quite difficult to be calculated, then a numerical
integration and binary search can be used instead.

The inverse transform sampling is a very well accepted method for
sampling uni-variate functions (one dimension). It can also be extended
to higher dimensions, but it demands more complex calculation steps. In
order to use it in higher dimensions it is necessary to calculate the
conditional cumulative distribution function. For instance, in the
bi-variate functions that would be written as

$$\begin{alignedat}{1}F_{XY}\left(x\mid y\right)=\dfrac{\iint p\left(x,y\right)dxdy}{\int p\left(x,y\right)dx}=\dfrac{F_{XY}\left(x,y\right)}{F_{Y}\left(y\right)}\end{alignedat}
,$$

where $F_{XY}\left(x,y\right)$ is the cumulative joint distribution and
$F_{Y}\left(y\right)$ is the cumulative marginal distribution. A random
sample $\left(x,y\right)$ can be obtained from two uniformly distributed
values, $u_{1}$ and $u_{2}$, in following manner,

$$\begin{alignedat}{1}y=F_{Y}^{-1}\left(u_{1}\right),\\
x=F_{X\mid Y}^{-1}\left(u_{2}\right).
\end{alignedat}$$

The calculation described above, for a two dimension function, can be
reasonable in terms of complexity and calculation time, but it degrades
rapidly when increasing the dimensionality. Therefore alternative
techniques are used for dimensions higher than one and some of them will
be reviewed in the following sections.

#### Rejection Sampling

In order to circumvent the drawbacks of sampling probability functions
with higher dimensions, a family of sampling techniques called Monte
Carlo Sampling is used. The most basic member of this family is the so
called rejection sampling. The main concept is to draw random values
from a known probability function where a sampling technique is also
known and, preferable, fast. This function, called proposal function and
represented by $Q\left(x\right)$, has to be larger than the target
probability function $p\left(x\right)$, i.e.
$Q\left(x\right)\geq p\left(x\right)$for any $x$. Let $X$ be a proposed
point sampled from $Q\text{\ensuremath{\left(x\right)}}$, then $X$ is
accepted as a random point of $p\left(x\right)$ with probability
$p\left(x\right)/Q\left(x\right)$. If the point is rejected then a new
point is proposed. The efficiency of this algorithm is given by the
overall accepting ratio that can be derived from the ratio between the
area under the target function and the area under the proposed function,
see figure [\[fig:rejectSamp\]](#fig:rejectSamp){reference-type="ref"
reference="fig:rejectSamp"}. This implies that good accepting ratios are
obtained when both propose and target distributions are similar.

\[Rejection sampling.\][\[fig:rejectSamp\]]{#fig:rejectSamp
label="fig:rejectSamp"}Illustration of the rejection sampling method.
The overall acceptance ratio is given by ratio between the area under
$p\left(x\right)$ and the area under $Q\left(x\right)$.

Some times, when the target distribution is too complex or there is no
*a priori* knowledge about it and, the uniform distribution can be used
as proposal distribution. Although the uniform distribution can be
efficiently sampled, it normally leads to a low efficiency in the
rejection sampling. This is exactly the case when sampling in the
orientation space.

#### Monte Carlo Markov Chain Sampling

The main limitation in the rejection sampling method is the need of
similar proposal function that is also easy to sample. A subclass of
algorithms called MCMC address this limitation by eliminating the need
of a user defined proposal distribution. The proposal distribution is
implicit created using the Markov chain principle. The Markov chain is
nothing else then a sequence of states (in this case points drawn from a
proposal distribution) where the current state depends on the previous
states. This means that points using only Monte Carlo technique are
independent while points using MCMC have some degree of correlation.

A pure Monte Carlo process is memoryless because it generates an
independent sequence of points. Whereas the MCMC uses the history
(correlation) to propose points with increased likelihood of being
accepted. For instance, to estimate what is the probability that a
standard normal random variable is less than $0.5$, one could generate
thousands of independent points following a standard normal distribution
and then count up the number lower than $0.5$. That would be a Monte
Carlo process. But if it is unknown how to draw independent normal
random variables, one could use a process that proposes a point by
adding some uniform random number between $-0.25$ and $0.25$ to the
current point. If the proposed point is a good candidate then it is kept
and promoted as current point otherwise the original current point is
kept and the process starts again. That is an MCMC process. The MCMC
introduces a local effect, if the current point $X$ is a good candidate
than its immediate neighborhood may also be. A sequence of points
generated in this way is called random walk.

Many MCMC algorithms have been developed but the two most populars are
the Metropolis-Hastings and Gibbs. Most of them are designed to work the
Euclidian space $\mathbb{R}^{n}$ which may cause problems when used for
sampling ODF's.

### New Method of Sampling in the Orientation Space

A new method of sampling distribution functions in the orientation space
is proposed here. The new method can efficiently sample arbitrary
distribution functions in the orientation space, works with any
representation of rotation (e.g. matrix or quaternion) and do not need
any specific data structure like regular grids. It is a modification of
a MCMC technique called slice sampling inspired by another MCMC
technique called hit and run sampling. Both techniques and the new
method are explained bellow.

#### Hit and Run

Hit and Run sampling was initially developed for creating uniform random
distributions in arbitrary closed domain. For instance, place random
points inside a pentagon or a star. Lately, it was modified for sampling
arbitrary probability functions. The original Hit and Run is described
in algorithm
[\[alg:basic-hit=000026run\]](#alg:basic-hit=000026run){reference-type="ref"
reference="alg:basic-hit=000026run"}. Note that the random direction $D$
and the the domain of the probability function
$p\left(x\right):\mathbb{R}^{n}\rightarrow\mathbb{R}^{+}$ have the same
dimension $\mathbb{R}^{n}$, e.g. two dimensions shapes have random
directions as unit vectors from a circle. A visual description of the
algorithm of 2D shapes is illustrated in figure
[\[fig:hit=000026run\]](#fig:hit=000026run){reference-type="ref"
reference="fig:hit=000026run"}.

\[Original Hit and Run
algorithm.\][\[fig:hit=000026run\]]{#fig:hit=000026run
label="fig:hit=000026run"}Visualization of the original Hit and Run
algorithm: (a) an arbitrary start point is given, (b) choose a random
direction, (c) pick a point at random distance, (d) if the point is
outside the domain then try it again and (e) final random walk.

The analogy is given by a snipper who shots at a random direction and
distance, if he hits the target then he moves to that position otherwise
he tries again. The sequence of points clearly generates a Markov chain.
The criteria of acceptance in the original design (uniform distribution
in closed domains) is trivial. If $D$ is the closed domain then $X$ is
only accepted if $X$ is inside the $D$. The modified version, for
sampling arbitrary probability functions, has a more complex sequence
that will not explained here. The advantage of this algorithm is the
ability of explorer the entire space and sample isolated regions. This
is important in the case of ODF's because them may have multiple
isolated peaks. The disadvantage is the need of tuning the range (the
maximum distance) where $k$ is sampled from. If $k$ is to small then the
output sequence is strong correlated, the random walk is quite local and
space might not be well explored. Whereas if $k$ is too large then
efficiency is quite low.

#### Slice sampling

Slice sampling is also a very popular MCMC sampling technique. It is
based on the principle that any region under any distribution function
have uniform density and, therefore, can be uniformly sampled, such a
region is a slice of the probability function at certain intensity.
Figure
[\[fig:sliceprinciple\]](#fig:sliceprinciple){reference-type="ref"
reference="fig:sliceprinciple"} illustrate this principle for a
probability function $p\left(x\right)$ in one dimension.

\[Principle of slice
sampling.\][\[fig:sliceprinciple\]]{#fig:sliceprinciple
label="fig:sliceprinciple"}Slice sampling is based on the principle that
a slice of the probability function $p\left(x\right)$ at a level $y$ can
be uniformly sampled.

In order to sample the target function the slice sampler randomly
alternate the slicing level covering the entire distribution function.
Algorithm [\[alg:basic-slice\]](#alg:basic-slice){reference-type="ref"
reference="alg:basic-slice"} describes a single iteration step to obtain
one sampled point. There are two crucial parts in this algorithm. The
first one, defined in the line
[\[basic-slice:getslice\]](#basic-slice:getslice){reference-type="ref"
reference="basic-slice:getslice"}, has to find the slice at given level
$y$. A slice is nothing else then the region enclosed by the level
set[^2], for instance, a slice in one dimension function is a set of
discrete points, in two dimensions it is a isoline (or contour line) and
in three dimensions it is a isosurface. Figure
[\[fig:slicebasic1D\]](#fig:slicebasic1D){reference-type="ref"
reference="fig:slicebasic1D"} illustrates the slice sampling for one
dimensional probability function.

\[Illustration of slice sampling in
1D.\][\[fig:slicebasic1D\]]{#fig:slicebasic1D label="fig:slicebasic1D"}
Illustration of slice sampling technique described in the algorithm
[\[alg:basic-slice\]](#alg:basic-slice){reference-type="ref"
reference="alg:basic-slice"} using one dimension probability function.

### References {#references .unnumbered}

mainmatter/bib/texsamp

Austenite Reconstruction
========================

Fully automated orientation relationship calculation
----------------------------------------------------

Prior austenite reconstruction
------------------------------

Two new methods, one for determining the experimentally observed OR and
another for reconstructing prior austenite phase, are proposed. Both
methods are based on the angular deviation of the OR at the grain
boundaries. The first algorithm identifies the optimum OR using the
misorientation distribution of the entire scan i.e. without manual
selection of parent grains. The second algorithm reconstructs the parent
phase using a random walk clustering technique that identifies groups of
closely related grains based on their angular deviation of the OR.

### Introduction {#introduction-5}

Most of the steelmaking process occurs at elevated temperature which,
for most steel grades, implies that part of the processing occurs in the
austenitic phase. The characterization of the high-temperature
austenitic phase is of great value to understand and control the
microstructure evolution during the entire production chain. But the
direct observation of the austenitic phase is not trivial. It can only
be done at elevated temperatures, and therefore, it can only be observed
within very specialized *in-situ* equipment like EBSD with a hot
stage[@seward_high-temperature_2002]. Moreover, such sophisticated
measurements are always done under simulating laboratory conditions,
which are not necessarily representative of the real process.

Alternatively, indirect measurements are done at the room temperature.
If, at this temperature, the phase transformation has already completed
then the prior austenite microstructure can observed by: (a) optical
microscopy with special etching techniques that revel the original grain
boundaries[@bechet_new_1955], (b) reconstruction of OIM obtained with
standard EBSD
equipment[@cayron_reconstruction_2006; @miyamoto_mapping_2010; @abbasi_approach_2012; @tari_back_2013; @bernier_alternative_2014].
The latter method identifies and groups product grains derived from a
single parent (austenitic) grain. This technique depends on the presence
of a specific OR between parent and product phases, and therefore, it
only applies to diffusionless phase transformations. This work
introduces two new methods aiming to improve the
current-state-of-the-art in prior austenite reconstruction.

### Orientation relationship calculation

One of the main characteristics of martensitic transformations is the
presence of a specific OR between parent and product phases. Although
there are some theoretical orientation relationships proposed such as
the KS or NW correspondences for steels, a more accurate OR can only be
derived from experimental data. The available
methods[@miyamoto_accurate_2009; @humbert_refinement_2011] for OR
identification depend on the manual selection of parent grains, which is
time consuming and in some cases, depending on the microstructure,
impractical.

The method proposed here can derive the OR from an orientation scan in a
fully automated manner and it is based on the misorientation between
neighboring grains. There are two possible situations where the OR can
be observed. The first one is between a parent grain and its product
grain (when still a residual fraction of the parent phase is present in
the product microstructure), cf. ([\[eq:1\]](#eq:1){reference-type="ref"
reference="eq:1"}). The second case is between product grains sharing
the same parent and involves a double OR transformation, cf.
([\[eq:2\]](#eq:2){reference-type="ref" reference="eq:2"}). In the ideal
conditions that the proposed OR is valid, the angular deviation
($\Delta OR$) is equal to $0$.

=\[font=\]

The algorithm finds the optimized OR transformation
($T^{\alpha'\leftrightarrow\gamma}$) that minimizes the average angular
deviation for all grain boundaries or for a selected subset of them. The
number of grain boundaries exhibiting this OR should be vastly larger
than the number of prior parent grain boundaries. This is a reasonable
assumption since the martensitic phase transformation produces grain
refinement and all the new grain boundaries have misorientations that
obey the OR transformation.

$$T_{m}^{\alpha'\leftrightarrow\gamma}\left(Og^{\alpha'}\right)\left(Og^{\gamma}\right)^{-1}=\Delta OR\label{eq:1}$$

$$T_{m}^{\alpha'\leftrightarrow\gamma}T_{n}^{\gamma\leftrightarrow\alpha'}\left(Og_{a}^{\alpha'}\right)\left(Og_{b}^{\alpha'}\right)^{-1}=\Delta OR\label{eq:2}$$

![[\[fig:recons\]]{#fig:recons label="fig:recons"}Validation of
reconstruction algorithm on Fe-Ni alloy: (a) initial OIM before
reconstruction, (b) retained austenite and expected prior austenite GB,
(c) reconstruction after the first iteration and (d) second and final
step of
reconstruction.](mainmatter/img/ausrec/sample1/alpha+gamma "fig:"){width="1\linewidth"}\
(a)

![[\[fig:recons\]]{#fig:recons label="fig:recons"}Validation of
reconstruction algorithm on Fe-Ni alloy: (a) initial OIM before
reconstruction, (b) retained austenite and expected prior austenite GB,
(c) reconstruction after the first iteration and (d) second and final
step of
reconstruction.](mainmatter/img/ausrec/sample1/RA+GB "fig:"){width="1\linewidth"}\
(b)

![[\[fig:recons\]]{#fig:recons label="fig:recons"}Validation of
reconstruction algorithm on Fe-Ni alloy: (a) initial OIM before
reconstruction, (b) retained austenite and expected prior austenite GB,
(c) reconstruction after the first iteration and (d) second and final
step of
reconstruction.](mainmatter/img/ausrec/sample1/1st+GB "fig:"){width="1\linewidth"}\
(c)

![[\[fig:recons\]]{#fig:recons label="fig:recons"}Validation of
reconstruction algorithm on Fe-Ni alloy: (a) initial OIM before
reconstruction, (b) retained austenite and expected prior austenite GB,
(c) reconstruction after the first iteration and (d) second and final
step of
reconstruction.](mainmatter/img/ausrec/sample1/2nd+GB "fig:"){width="1\linewidth"}\
(d)

Two strategies were employed in order to calculate the grain boundary
misorientations. One is based on the average orientation of both
neighboring grains. The other one is based on the orientation in the
immediate vicinity of the grain boundary, where only the first layer of
orientations at each side of the grain boundary is selected. Slightly
different values of OR are obtained by the two strategies due to plastic
strain accommodation during phase transformation and associated
orientation gradients in bulk of the grains[@miyamoto_accurate_2009].

This new method was validated with a low Nb steel alloy with carbon
content around $0.05\%$.
Figure [\[fig:ORhist\]](#fig:ORhist){reference-type="ref"
reference="fig:ORhist"} compares the distribution of angular deviation
using the theoretical KS OR and the optimized OR, it can be noticed that
the distribution shifts to the left, i.e. reducing the overall error,
when the optimized OR is used. Better results are obtained when
considering orientations in the immediate vicinity of the grain boundary
compared to considering grain averaged orientations.
Figure [\[fig:ORcomp\]](#fig:ORcomp){reference-type="ref"
reference="fig:ORcomp"} compares the optimized OR for the low Nb sample
with two theoretical ORs and one reference
value[@miyamoto_accurate_2009] that contains comparable amount of
carbon. The optimized OR is in good agreement with the reference value.

### Parent phase reconstruction

A new method, equally based on grain boundary misorientations, is
proposed for the parent phase reconstruction. The key concept of this
method is the use of a graph clustering algorithm to find groups of
closely related grains that originate from a single parent grain. The
Markov Clustering algorithm[@dongen_graph_2001; @schaeffer_graph_2007]
is used because of the following features:

-   It is based on the probabilities of random walks through the graph.

-   It identifies natural cuts and clusters in the graph without the
    need to specify threshold values of allowable angular deviations or
    the necessity to specify a predefined number of clusters

-   There is only one parameter that controls the cluster coarsening.

-   Excellent noise tolerance.

-   Fast calculation time.

In order to use the clustering algorithm, a graph has to be derived from
the OIM in the following way: (a) each grain, product or parent, becomes
a node in the graph, (b) each grain boundary becomes an edge connecting
two neighboring nodes (grains) and (c) each edge receives a value based
on the equation ([\[eq:1\]](#eq:1){reference-type="ref"
reference="eq:1"}) or ([\[eq:2\]](#eq:2){reference-type="ref"
reference="eq:2"}), which determines the probability of the two
neighboring grains being products of the same parent. Once the graph is
constructed, the clusters are determined using the highest level of
coarsening. Then each cluster forms a parent grain and its orientation
is calculated by an optimization algorithm that minimizes the average
angular deviation using the
equation ([\[eq:1\]](#eq:1){reference-type="ref" reference="eq:1"}).

The first iteration described above may result in some of the clusters
containing more than one parent grain because of the initial coarsening
level. These grains can be identified by its higher average angular
deviation, and therefore, they should be further refined by running an
extra iteration step with reduced coarsening level. The refinement
procedure should be repeated many times until all parent grains have low
average error.

A Fe-Ni 29% sample was used to validate the proposed reconstruction
method. The OIM after the martensitic phase transformation with still
10% retained austenite is shown in the
figure [\[fig:recons\]](#fig:recons){reference-type="ref"
reference="fig:recons"}a. The expected parent microstructure is
visualized in figure [\[fig:recons\]](#fig:recons){reference-type="ref"
reference="fig:recons"}b by the retained austenite and the traces of
high angular deviation ($>10^{\circ}$).
Figure [\[fig:recons\]](#fig:recons){reference-type="ref"
reference="fig:recons"}c shows the reconstruction after the first
iteration, showing that some reconstructed grains incorporate more than
one parent grain. The final microstructure is shown in
figure [\[fig:recons\]](#fig:recons){reference-type="ref"
reference="fig:recons"}d after an extra step of refinement.

A careful look at the
figure [\[fig:recons\]](#fig:recons){reference-type="ref"
reference="fig:recons"}d shows that some unexpected features are visible
at twin boundaries in parent grains and some grains are over refined.
The current implementation of the reconstruction software is quite
simple and does not include any special rule for treating this type of
boundaries or for re-merging over-divided clusters.

### Conclusion

Tolerance error reduction and noise immunity are two fundamental
strategies for the success of prior austenite reconstruction. The use of
optimized ORs reduces the overall error but the identification by
previous techniques requires manual selection of parent grains. The
method proposed here identifies an optimized OR, whereby the entire
orientation scan is considered and the result is obtained without user
intervention.

Markov clustering seems a promising technique for parent grain
reconstruction due its noise tolerance and fast calculation time. The
present results are satisfactory considering the initial stage of
development and the simple implementation. Further work on the special
treatment of twin boundaries and cluster re-merging will be done.

### References {#references-1 .unnumbered}

mainmatter/bib/ausrec

Model Validation and Application
--------------------------------

Appendix
========

Delunay Triangulation
---------------------

$SD\left(D\right)=[m\left(D\right)-m_{<A,B,C>}]\cdot\dfrac{\left(a-b\right)\times\left(a-c\right)}{\parallel\left(a-b\right)\times\left(a-c\right)\parallel}$

$\left[\begin{array}{cccc}
1 & a_{x} & a_{y} & a_{z}\\
1 & b_{x} & b_{y} & b_{z}\\
1 & c_{x} & c_{y} & c_{z}\\
1 & d_{x} & d_{y} & d_{z}
\end{array}\right]\left[\begin{array}{c}
\parallel m\left(D\right)\parallel^{2}-w_{m}\\
-2m_{x}\left(D\right)\\
-2m_{y}\left(D\right)\\
-2m_{z}\left(D\right)
\end{array}\right]=-\left[\begin{array}{c}
\parallel a\parallel^{2}-w_{a}\\
\parallel b\parallel^{2}-w_{b}\\
\parallel c\parallel^{2}-w_{c}\\
\parallel d\parallel^{2}-w_{d}
\end{array}\right]$

$\left[\begin{array}{ccc}
b_{x}-a_{x} & b_{y}-a_{x} & b_{z}-a_{x}\\
c_{x}-a_{x} & c_{y}-a_{x} & c_{z}-a_{x}\\
d_{x}-a_{x} & d_{y}-a_{x} & d_{z}-a_{x}
\end{array}\right]\left[\begin{array}{c}
-2m_{x}\left(D\right)\\
-2m_{y}\left(D\right)\\
-2m_{z}\left(D\right)
\end{array}\right]=-\left[\begin{array}{c}
\parallel b\parallel^{2}-\parallel a\parallel^{2}-w_{b}+w_{a}\\
\parallel c\parallel^{2}-\parallel a\parallel^{2}-w_{c}+w_{a}\\
\parallel d\parallel^{2}-\parallel a\parallel^{2}-w_{d}+w_{a}
\end{array}\right]$

$M^{T}[-2m\left(D\right)]=-\alpha$

where

$M=\left[\begin{array}{ccc}
b_{x}-a_{x} & c_{x}-a_{x} & d_{x}-a_{x}\\
b_{y}-a_{y} & c_{y}-a_{y} & d_{y}-a_{y}\\
b_{z}-a_{z} & c_{z}-a_{z} & d_{z}-a_{z}
\end{array}\right]$ $\alpha=\left[\begin{array}{c}
\parallel b\parallel^{2}-\parallel a\parallel^{2}-w_{b}+w_{a}\\
\parallel c\parallel^{2}-\parallel a\parallel^{2}-w_{c}+w_{a}\\
\parallel d\parallel^{2}-\parallel a\parallel^{2}-w_{d}+w_{a}
\end{array}\right]$

let $\left(Q,R\right)=qrDecomp\left(M\right)$

$M^{T}[-2m\left(D\right)]=R^{T}[-2Q^{T}m\left(D\right)]=R^{T}\mu$

$m\left(D\right)=-\frac{1}{2}Q\mu=-\frac{1}{2}\left(q_{1}\mu_{x}+q_{2}\mu_{y}+q_{3}\mu_{z}\right)$

where $\mu$ can be easily solved since it is a lower triangular matrix

$R^{T}\mu=-\alpha$

$\mu_{x}=\dfrac{-\alpha_{x}}{r_{11}}$

$\mu_{y}=\dfrac{-\alpha_{x}-\mu_{x}r_{12}}{r_{22}}$

$\mu_{z}=\dfrac{-\alpha_{z}-\mu_{x}r_{13}-\mu_{y}r_{23}}{r_{33}}$

Misorientation definition
-------------------------

Rotations can be seen as functions that map one vector space into an
other.

$$g\,:\,s\rightarrow t$$

Rotations can be composed using function composition.

$$g_{sv}=g_{tv}\cdot g_{st}$$

where $g_{sv}\,:\,s\rightarrow v$; $g_{tv}\,:\,t\rightarrow v$;
$g_{st}\,:\,s\rightarrow t$.

Let $g_{A}$ and $g_{B}$ be two rotations that bring the reference frame
to the frames A and B, respectively. And $\Delta g_{AB}$ the rotation
that brings the frame A to B.

$$g_{B}=\Delta g_{AB}\cdot g_{A}$$

$$g_{B}\cdot g_{A}^{-1}=\Delta g_{AB}\cdot g_{A}\cdot g_{A}^{-1}$$

$$g_{B}\cdot g_{A}^{-1}=\Delta g_{AB}$$

It also exist an inverse rotation that brings B to A, $\Delta g_{BA}$.
In such a case,

$$g_{A}=\Delta g_{BA}\cdot g_{B}$$

$$g_{A}\cdot g_{B}^{-1}=\Delta g_{BA}\cdot g_{B}\cdot g_{B}^{-1}$$

$$g_{A}\cdot g_{B}^{-1}=\Delta g_{BA}$$

Rotation compositions are not commutative, so

$$g_{B}\cdot g_{A}^{-1}\neq g_{A}^{-1}\cdot g_{B}$$

If we enforce the first element to be the inverse

$$g_{A}^{-1}\cdot g_{B}=g_{A}^{-1}\cdot\Delta g_{AB}\cdot g_{A}$$

$$\begin{aligned}
g_{A}^{-1}\cdot g_{B} & = & \overline{\Delta g_{AB}}\end{aligned}$$

then the result will be the conjugate of the misorientation, which, if
coverted to axis-rotation pair, will result the same rotation $\omega$
but with different axis from $\Delta g_{AB}$.

> Definition 2.12 (conjugate elements): If $a$ and $b$ are elements of
> $G$, then $a$ and $b$ are conjugate elements in $G$ if there exists an
> element $g$ such that $b=gag^{-1}$ . If $a$ and $b$ are conjugate, we
> write $a\backsim b$.
>
> This notion of conjugation is of great importance to physics
> applications, since conjugation is essentially a similarity
> transformation. For example, in the rotation group, if $a$ denotes a
> rotation by $30^{\circ}$ about the $x$-axis, then any rotation by
> $30^{\circ}$ is conjugate to a since rotations about different axes
> are related by a similarity transformation.[^3]

On the other hand, the relation between both misorientations is given by

$$g_{B}\cdot g_{B}^{-1}=\Delta g_{AB}\cdot g_{A}\cdot g_{B}^{-1}$$

$$I=\Delta g_{AB}\cdot g_{A}\cdot g_{B}^{-1}=\Delta g_{AB}\cdot\Delta g_{BA}$$

$$\Delta g_{AB}=(\Delta g_{BA})^{-1}$$

Colophon {#colophon .unnumbered}
--------

This document was typeset using the typographical look-and-feel
`classicthesis`

[^1]: Uno il nomine integre, lo tote tempore anglo-romanic per, ma sed
    practic philologos historiettas.

[^2]: Mathematically, level set is defined as the set of points where a
    function has same predefined value. It has the form
    $L_{y}\left(p\right)=\left\{ x\mid p\left(x\right)=y\right\}$.

[^3]: 2 Subgroups and conjugate elements.
    http://pauli.physics.lsa.umich.edu/p452/gt02.pdf\
    see, Group Theoretical Methods and Applications to Molecules and
    Crystals, page 62
