BeginPackage["DataFederation`"]

ToPartitionSheet::usage =
  "ToSpreadSheet[_List,___Options]"

Begin["Private`DataFederation`"]

ToPartitionSheet["$$$3[]","$$$2002[]","$$$2004[,]"]["xlsx"][list_]:= Module[
  {itemNames,unitNames,values,rl,rls,lines,cols},
  itemNames = list[[1,1]];
  unitNames = list[[1,2]];
  values = Drop[list[[1]],2];
  rls = Map[  (rl[#][v_] := Rule[ itemNames[[#]], Quantity[ v,unitNames[[#]] ] ])&, Table[i,{i,Length[itemNames]}]  ];
  {lines, cols} = Dimensions[values];
  Transpose[Table[rl[i][values[[j, i]]], {i, cols}, {j, lines}]]
];

End[];
EndPackage[];
