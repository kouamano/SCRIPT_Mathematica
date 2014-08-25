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
cDB[edMat_, dshortMatZeroself_, cls_, medoids_] := 
  Tr[Table[r[edMat, dshortMatZeroself, cls, medoids, i], {i, 
    Length[cls]}]]
cDB[edMat_, dshortMatZeroself_, cls_] := Module[{sampleMedoids},
  sampleMedoids = (orgIndexFromMedoidParCL[fc, findMedoidsParCL[edMat, cls]] // Flatten);
  Tr[Table[
    r[edMat, dshortMatZeroself, cls, sampleMedoids, i], {i, Length[cls]}]]
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


End[]
EndPackage[]
