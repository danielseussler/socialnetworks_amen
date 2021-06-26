**The Alliances Data Set**

Description The alliances dataset contains the international defense alliance
network among 164 countries, covering the years 1981–2000. In addition to the
yearly defense alliance network, it contains data on military capabilities,
governing regime type, geographic contiguity and international conflict. This
is an excerpt from a dataset that has been used in two published analyses. The
full dataset (Cranmer, Desmarais and Menninga 2012; Cranmer, Desmarais and
Kirlkand 2012) contains a large number of countries and a much longer time
series.

**allyNet** is a list of network objects at 20 time points, 1981–2000, containing
undirected defense alliance networks. In addition to the alliance ties, each
network object contains three vertex attributes. cinc is the "CINC" or
Composite Index of National Capability score (see
https://correlatesofwar.org/data-sets/national-material-capabilities). polity
is the "polity score" of each country in the respective year. Quoting the
online description, "the Polity Score captures this regime authority spectrum
on a 21-point scale ranging from -10 (hereditary monarchy) to +10
(consolidated democracy)," (see
http://www.systemicpeace.org/polityproject.html). year is simply the year
recorded as a vertex attribute.

**contigMat** is a 164 x 164 binary matrix in which a 1 indicates that two
countries share a border.

**lNet** is a list of 20 matrices. Each element is the adjacency matrix from the
previous year. This is used to model memory in the ties.

**LSP** is a list of 20 matrices. Each element is a matrix recording the number of
shared partners between countries in the alliance network from the previous
year.

**warNet** is a list of 20 matrices. Each element is a binary matrix that
indicates whether two states were in a militarized interstate dispute in the
respective year.

COW Countrycodes: [COW Country Codes — Correlates of War](https://correlatesofwar.org/data-sets/downloadable-files/cow-country-codes/cow-country-codes/view)
