(*load other package*)
Needs["Combinatorica`"]

BeginPackage["ConnectivityClass`"]
test::usage = "test[x]"
(*function usage*)

Begin["`Private`"]
test[x_] := x^5
(*function code*)

End[]
EndPackage[]
