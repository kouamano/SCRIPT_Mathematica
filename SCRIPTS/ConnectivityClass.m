(** Copyright **)
(*This package is based on "ClusterValidityIndices_Sriparna.nb".*)
(*The argorithms were developed by Sriparna, et.al.*)



(*load other package*)
Needs["Combinatorica`"]



BeginPackage["ConnectivityClass`"]
test::usage = "test[x]"
(*function usage*)
(**基本**)
subDMat::usage = "距離行列から特定のクラスタに関する部分距離行列を抜き出す。"
argMinDMat::usage = "部分距離行列をもとに特定のクラスタのargminを求める(minindex)。"
findMedoidsParCL::usage = "距離行列とクラスタリング結果から各クラスタのmedoidをもとめる。"
orgIndexFromMedoidParCL::usage = "各クラスタのmedoid番号からもとのサンプル番号を求める。"
dropDiagonal::usage = "行列から対角要素をdropする(サイズは縮小)。"
(**cDB**)
cDB::usage = "cDB = Sum(R[i],{i,1,K}) / K   ; K:クラスタ数
R[i] = Max[{j,j!=i}]((S[i]+S[j])/d[i,j])   ; i:クラスタi, d[i,j]: ユークリッド距離行列
S[i] = Sum(dshort(x,z[i]),{x,x\[Element]C[i]}) / n[i]   ; z[i]:medoid of cluster i, n[i]:number of cluster points."
(**cDunn**)
cDunn::usage = "cDunn = Min[i, 1, K] (Min[j,1,K ; j!=i]( d(C[i],C[j])/Max[k,1,K](delta(C[k])) ))
delta(C) = Max[x,y \[Element] C](dshort(x,y)) ; diameter
d(C[i],C[j]) = Min[x\[Element]C[i] , y\[Element]C[j]](dshort(x,y))
C : クラスタ、K : クラスタ数"
(**cGDunn**)
cGDunn::usage = "cGDunn = Min[s, 1, K](Min[t,1,K ; t!=s]( Gd(C[s],C[t])/Max[k,1,K](Gdelta(C[k])) )) .
Gdelta(S) = 2 (Sum[x\[Element]S](dshort(x,z[S]))) / Length(S) ; z[S] : medoid of S ; S : クラスタ .
Gd(S,T) = 1 / (Length(S) Length(T)) Sum[x\[Element]S,y\[Element]T](dshort(x,y)) : S,T : クラスタ ."
(**cPS**)
cPS::usage = "cPS=1/K \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(K\)]\(
\*FractionBox[\(1\), \(n[i]\)] \(
\*UnderscriptBox[\(\[Sum]\), \(x \[Element] S[i]\)]
\*FractionBox[\(dshort \((x, z[i])\)\), \(dmin\)]\)\)\)
dmin = Min[n,1,K; m,1,K; n!=m](dshort(z[n],z[m])) ; z[n]: medoid of cluster n , n[i]: num of members of cluster S[i], K: num of clusters, S[i]: cluster S[i]. "
(**cI**)
cI::usage = "cI = 1/K* 1/e[K] * D[K]
e[K] = sum[ i,1,K ]( lE[i] )
lE[i] = sum[ j,1,n[i] ]( dshort(x[i][j],z[i]) )
D[K] = max[ i,1,K ; j,1,k ]( dshort(z[i],z[j]) )
where
n[i]: num of members of cluster i
z[i]: medoid of cluster i"
(**cXB**)
cXB::usage = "cXB = (sum[i,1,K](sum[x\[Element]C[i]](dshort(x,z[i])))) / (n min[ i,1,K ; j,1,K ; i!=j](dshort(z[i],z[j])))"
(**cSV**)
cSV::usage = "cSV = 1/K * sum[i,1,K](sum[ x\[Element]C[i] ]( dshort(x,z[i])/n[i] )) + K/dmin
dmin = min[ z:medoids ; i!=j ](dshort(z[i],z[j]))
n[i] : number of members of cluster i"


Begin["`Private`"]
test[x_] := x^5
(*function code*)
(**基本**)
subDMat[dmat_, members_] := 
  Transpose[Transpose[dmat[[members]]][[members]]]
argMinDMat[dmat_] := Module[{sumList},
  sumList = Map[Tr[#] &, dmat];
  Position[sumList, Min[sumList]][[1]]
]
findMedoidsParCL[dmat_, clusterResult_] := 
  Map[argMinDMat[subDMat[dmat, #]] &, clusterResult]
orgIndexFromMedoidParCL[clusterResult_, medoids_] := Module[
  {l, fmedoids},
  l = Length[medoids];
  fmedoids = Flatten[medoids];
  Table[clusterResult[[i]][[medoids[[i]]]], {i, l}]
]
dropDiagonal[mat_] := Table[Drop[mat[[n]], {n}], {n, Length[mat]}]
(**cDB**)
s[dshortMatZeroself_, clMembers_, medoidIndex_] := 
  Tr[dshortMatZeroself[[medoidIndex, clMembers]]]/Length[clMembers]
r[edMat_, dshortMatZeroself_, cls_, medoids_, i_] := Module[{js},
  js = Drop[Range[Length[cls]], {i}];
  Max[  Map[(s[dshortMatZeroself, cls[[i]], medoids[[i]]] + 
        s[dshortMatZeroself, cls[[#]], medoids[[#]]])/edMat[[i, #]] &, js]  
  ]
]
cDB[edMat_, dshortMatZeroself_, cls_] := Module[{sampleMedoids},
  sampleMedoids = (orgIndexFromMedoidParCL[fc, findMedoidsParCL[edMat, cls]] // Flatten);
  Tr[Table[
    r[edMat, dshortMatZeroself, cls, sampleMedoids, i], {i, 
    Length[cls]}]]
]
(**cDunn**)
d[cl1_, cl2_, dshortMatZeroself_] := Module[
  {outer},
  outer = Flatten[Outer[List, cl1, cl2, 1], 1];
  Min[Map[dshortMatZeroself[[#[[1]], #[[2]]]] &, outer]]
]
delta[cl_, dshortMatZeroself_] := Module[
  {subset},
  subset = Subsets[cl, {2}];
  If[Length[subset] == 0, 0, 
    Max[Map[dshortMatZeroself[[#[[1]], #[[2]]]] &, subset]]]
]
cDunn[cls_, dshortMatZeroself_] := Module[
  {numCls, maxdelta, subset},
  numCls = Length[cls];
  maxdelta = Max[Map[delta[#, dshortMatZeroself] &, cls]];
  subset = Subsets[cls, {2}];
  Min[Map[d[#[[1]], #[[2]], dshortMatZeroself] &, subset]]/maxdelta
]
(**cGDunn**)
Gd[s_List, t_List, dshortMatZeroself_List] := 
  1/(Length[s] Length[t]) Tr[
    Flatten[Table[
      dshortMatZeroself[[s[[x]], t[[y]]]], {x, Length[s]}, {y, Length[y]}]]]
Gdelta[s_List, medID_, dshortMatZeroself_List] := 
  2 Tr[Table[dshortMatZeroself[[s[[x]], medID]], {x, Length[s]}]]/ 
    Length[s]
cGDunn[cls_List, dshortMatZeroself_List] := 
  Module[{medIDs, maxGdelta, subset},
    medIDs = (orgIndexFromMedoidParCL[cls, 
      findMedoidsParCL[dshortMatZeroself, cls]] // Flatten);
  maxGdelta = 
    Max[Table[
      Gdelta[cls[[i]], medIDs[[i]], dshortMatZeroself], {i, 
      Length[cls]}]];
  subset = Subsets[cls, {2}];
  Min[Map[Gd[#[[1]], #[[2]], dshortMatZeroself] &, subset]]/maxGdelta
]
(**cPS**)
dMin[dshortMatZeroself_, cls_] := Module[
  {medoids, subdmat},
  medoids = 
    Flatten[orgIndexFromMedoidParCL[cls, 
      findMedoidsParCL[dshortMatZeroself, cls]]];
  subdmat = subDMat[dshortMatZeroself, medoids];
  Min[dropDiagonal[subdmat]]
]
(**cI**)
cI[dshortMatZeroself_, cls_] := Module[
  {size, eK, dK, subdmats, medoidsOfCl, sampleIDsOfmedoids, medDmat},
  size = Length[cls];
  subdmats = Map[subDMat[dshortMatZeroself, #] &, cls];
  medoidsOfCl = Flatten[findMedoidsParCL[dshortMatZeroself, cls]];
  eK = Tr[
    Table[Tr[subdmats[[n]][[medoidsOfCl[[n]]]]], {n, 
      Length[medoidsOfCl]}]];
  sampleIDsOfmedoids = orgIndexFromMedoidParCL[cls, medoidsOfCl];
  medDmat = subDMat[dshortMatZeroself, sampleIDsOfmedoids];
  dK = Max[medDmat];
  (1/size) (1/eK) dK
]
(**cXB**)
cXB[dshortMatZeroself_, cls_] := Module[
  {size, numWholeSamples, subdmats, medoidsOfCl, medDmat, 
  sampleIDsOfmedoids, numerator, denominator},
  size = Length[cls];
  numWholeSamples = Length[dshortMatZeroself];
  subdmats = Map[subDMat[dshortMatZeroself, #] &, cls];
  medoidsOfCl = Flatten[findMedoidsParCL[dshortMatZeroself, cls]];
  numerator = 
    Tr[Flatten[Table[subdmats[[n]][[medoidsOfCl[[n]]]], {n, size}]]];
  sampleIDsOfmedoids = orgIndexFromMedoidParCL[cls, medoidsOfCl];
  medDmat = subDMat[dshortMatZeroself, sampleIDsOfmedoids];
  denominator = (Min[dropDiagonal[medDmat]] size);
  numerator/denominator
]
(**cSV**)
cSV[dshortMatZeroself_, cls_] := Module[
  {size, numsClMems, subdmats, medoidsOfCl, vu, sampleIDsOfmedoids, 
  meddmat, dmin},
  size = Length[cls];
  numsClMems = Map[Length[#] &, cls];
  subdmats = Map[subDMat[dshortMatZeroself, #] &, cls];
  medoidsOfCl = Flatten[findMedoidsParCL[dshortMatZeroself, cls]];
  vu = (Tr[
    Table[Tr[subdmats[[n]][[medoidsOfCl[[n]]]]/numsClMems[[n]]], {n,
    size}]]/size);
  sampleIDsOfmedoids = orgIndexFromMedoidParCL[cls, medoidsOfCl];
  meddmat = subDMat[dshortMatZeroself, sampleIDsOfmedoids];
  dmin = Min[dropDiagonal[meddmat]];
  vu + dmin
]


End[]
EndPackage[]
