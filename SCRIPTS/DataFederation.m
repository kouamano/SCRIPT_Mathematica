BeginPackage["DataFederation`"]

toPartitionSheet::usage =
  "ToSpreadSheet[_List,___Options]"

Begin["Private`DataFederation`"]

toPartitionSheet[List["$X$List[$$$3[DIM,]]","$X$List[$$$2002[DIM,]]","$X$List[DIM,][$$$2004[DIM,]]"]]["$$3020"][list_]:= Module[
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
