(* ::Package:: *)

(* ::Title:: *)
(*SpinorHelicity*)


BeginPackage["SpiniorHelicity`"]


(* ::Section:: *)
(*Messages*)


SpinorAngleBracket::usage = "SpinorAngleBracket[\*StyleBox[\"x\", \"TI\"], \*StyleBox[\"y\", \"TI\"]] represents the contraction of undotted spinors \!\(\*SubscriptBox[\(\[Lambda]\), \(x\)]\) and \!\(\*SubscriptBox[\(\[Lambda]\), \(y\)]\)."
SpinorAngleBracketBox::usage = "The box associated with angle brackets."
SpinorSquareBracket::usage = "SpinorSquareBracket[\!\(\*StyleBox[\"x\", \"TI\"]\), \!\(\*StyleBox[\"y\", \"TI\"]\)] represents the contraction of undotted spinors \!\(\*SubscriptBox[OverscriptBox[\(\[Lambda]\), \(~\)], \(x\)]\) and \!\(\*SubscriptBox[OverscriptBox[\(\[Lambda]\), \(~\)], \(y\)]\)."
SpinorSquareBracketBox::usage = "The box associated with square brackets."
SpinorRoundBracket::usage = "SpinorRoundBracket[\*StyleBox[\"x\", \"TI\"], \*StyleBox[\"y\", \"TI\"]] represents the contraction of two generic spinors."
SpinorRoundBracketBox::usage = "The box associated with round brackets."
SpinorUndottedBox::usage = "The box associated with undotted spinors."
SpinorUndotted::usage = "SpinorUndotted[\*StyleBox[\"x\", \"TI\"]] represents the undotted spinor \!\(\*SubscriptBox[OverscriptBox[\(\[Lambda]\), \(~\)], \(x\)]\)."
SpinorDottedBox::usage = "The box associated with dotted spinors."
SpinorDotted::usage = "SpinorDotted[\*StyleBox[\"x\", \"TI\"], \*StyleBox[\"y\", \"TI\"]] represents the undotted spinor \!\(\*ubscriptBox[OverscriptBox[\"\[Lambda]\", \"~\"], \(x\)]\)."
SpinorContract::usage = "SpinorContract[\*SubscriptBox[\"e\", \"1\"], \*SubscriptBox[\"e\", \"2\"]] contracts free spinor indices between expressions \*SubscriptBox[\"e\", \"1\"] and \*SubscriptBox[\"e\", \"2\"]."
Momenta::usage = "Momenta[\*StyleBox[\"expr\", \"TI\"]] extracts the name of the momenta appearing in brackes in \*StyleBox[\"expr\", \"TI\"]."
SpinorReplace::usage = "SpinorReplace[\*StyleBox[\"expr\", \"TI\"], \!\(\*SubscriptBox[\(\[Lambda]\), \(a\)]\)\[Rule]\*StyleBox[\"sp\", \"TI\"]] replaces occurrences of \!\(\*SubscriptBox[\(\[Lambda]\), \(a\)]\) with an undotted spinor expression \*StyleBox[\"sp\", \"TI\"] in angle spinor brackets appearing in \*StyleBox[\"expr\", \"TI\"].\nSpinorReplace[\!\(\*StyleBox[\"expr\", \"TI\"]\), \!\(\*SubscriptBox[OverscriptBox[\(\[Lambda]\), \(~\)], \(a\)]\)\[Rule]\!\(\*StyleBox[\"sp\", \"TI\"]\)] replaces occurrences of \!\(\*SubscriptBox[OverscriptBox[\(\[Lambda]\), \(~\)], \(a\)]\) with a dotted spinor expression \!\(\*StyleBox[\"sp\", \"TI\"]\) in square spinor brackets appearing in \!\(\*StyleBox[\"expr\", \"TI\"]\).\nSpinorReplace[\!\(\*StyleBox[\"expr\", \"TI\"]\), \!\(\*StyleBox[\"rules\", \"TI\"]\)] applies the list \!\(\*StyleBox[\"rules\", \"TI\"]\) to \!\(\*StyleBox[\"expr\", \"TI\"]\).\nSpinorReplace[\!\(\*StyleBox[\"rules\", \"TI\"]\)] represents an operator form of SpinorReplace that can be applied to an expression."
SpinorWeight::usage = "SpinorWeight[\*StyleBox[\"expr\", \"TI\"], \*StyleBox[\"a\", \"TI\"]] computes the weight of momentum \*StyleBox[\"a\", \"TI\"] in \*StyleBox[\"expr\", \"TI\"] in terms of its little group transformation."
MassDimension::usage = "MassDimension[\*StyleBox[\"expr\", \"TI\"]] computes the mass dimension of \*StyleBox[\"expr\", \"TI\"], where \*StyleBox[\"expr\", \"TI\"] is a function of angle and square spinor brackets."
MomentumSquare::usage = "SpinorSquare[{\*SubscriptBox[\"m\", \"1\"], \*SubscriptBox[\"m\", \"2\"], \[Ellipsis]}] computes the square of the sum of on shell momenta \*SubscriptBox[\"m\", \"1\"], \*SubscriptBox[\"m\", \"2\"], \[Ellipsis] in terms of angle and square spinor brackets."
SpinorSimplify::usage = "SpinorSimplify[\!\(\*StyleBox[\"expr\", \"TI\"]\)] simplifies the expression \!\(\*StyleBox[\"expr\", \"TI\"]\) by using Schouten identities.\nSpinorSimplify[\!\(\*StyleBox[\"expr\", \"TI\"]\), {{\!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \[Ellipsis], \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[\"m\", \"TI\"]]\)},{\!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"m\", \"+\", \"1\"}], \"TI\"]]\), \[Ellipsis], \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[\"n\", \"TI\"]]\)}}] simplifies the expression \!\(\*StyleBox[\"expr\", \"TI\"]\) by using Schouten identities and momentum conservation \!\(\*SubscriptBox[StyleBox[\"p\", \"TI\"], SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[\"1\", \"TR\"]]]\) + \[Ellipsis] + \!\(\*SubscriptBox[StyleBox[\"p\", \"TI\"], SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[\"m\", \"TI\"]]]\) - \!\(\*SubscriptBox[StyleBox[\"p\", \"TI\"], SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"m\", \"+\", \"1\"}], \"TI\"]]]\) - \[Ellipsis] - \!\(\*SubscriptBox[StyleBox[\"p\", \"TI\"], SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[\"m\", \"TI\"]]]\) = 0.\nSpinorSimplify[\!\(\*StyleBox[\"expr\", \"TI\"]\), {\!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \[Ellipsis], \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[\"m\", \"TI\"]]\)}] is a shorthand for SpinorSimplify[\!\(\*StyleBox[\"expr\", \"TI\"]\), {{\!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \[Ellipsis], \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[\"m\", \"TI\"]]\)},{}}].\nSpinorSimplify[\!\(\*StyleBox[\"expr\", \"TI\"]\), {{\!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \[Ellipsis], \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[\"m\", \"TI\"]]\)},{\!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[RowBox[{\"m\", \"+\", \"1\"}], \"TI\"]]\), \[Ellipsis], \!\(\*SubscriptBox[StyleBox[\"i\", \"TI\"], StyleBox[\"n\", \"TI\"]]\)}}, {{\!\(\*SubscriptBox[StyleBox[\"j\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \[Ellipsis], \!\(\*SubscriptBox[StyleBox[\"j\", \"TI\"], StyleBox[\"m\", \"TI\"]]\)},{\!\(\*SubscriptBox[StyleBox[\"j\", \"TI\"], StyleBox[RowBox[{\"m\", \"+\", \"1\"}], \"TI\"]]\), \[Ellipsis], \!\(\*SubscriptBox[StyleBox[\"j\", \"TI\"], StyleBox[\"n\", \"TI\"]]\)}}, \[Ellipsis]] allows to use multiple momentum conservation identities at the same time."
ParkeTaylor::usage = "ParkeTaylor[\"Angle\"] is a functional that takes a list of momenta and produces the associated Parke\[LongDash]Taylor denominator in terms of angle brackets.\nParkeTaylor[\"Square\"] is a functional that takes a list of momenta and produces the associated Parke\[LongDash]Taylor denominator in terms of square brackets.\nParkeTaylor[\"Round\"] is a functional that takes a list of momenta and produces the associated Parke\[LongDash]Taylor denominator in terms of round brackets."


(* ::Section:: *)
(*Private*)


Begin["`Private`"]


(* ::Subsection:: *)
(*SpinorAngleBracket*)


SpinorAngleBracket[a_, b_] /; (a == b) := 0
SpinorAngleBracket[a_, b_] /; \[Not]OrderedQ[{a,b}] := -SpinorAngleBracket[b, a]

SpinorAngleBracketBox[a_, b_] :=
    TemplateBox[{a, b}, "SpinorAngleBracket",
        DisplayFunction -> (RowBox[{"\[LeftAngleBracket]",RowBox[{#1,",",#2}],"\[RightAngleBracket]"}]&),
        InterpretationFunction -> (RowBox[{"SpinorAngleBracket","[",RowBox[{#1,",",#2}],"]"}]&)]

SpinorAngleBracket /: MakeBoxes[SpinorAngleBracket[a_, b_], StandardForm | TraditionalForm] := SpinorAngleBracketBox[ToBoxes[a], ToBoxes[b]]

SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "ab" -> SpinorAngleBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"]]]


(* ::Subsection:: *)
(*SpinorSquareBracket*)


SpinorSquareBracket[a_, b_] /; (a == b) := 0
SpinorSquareBracket[a_, b_] /; \[Not]OrderedQ[{a, b}] := -SpinorSquareBracket[b, a]

SpinorSquareBracketBox[a_, b_] :=
    TemplateBox[{a, b}, "SpinorSquareBracket",
        DisplayFunction -> (RowBox[{"[",RowBox[{#1,",",#2}],"]"}]&),
        InterpretationFunction -> (RowBox[{"SpinorSquareBracket","[",RowBox[{#1,",",#2}],"]"}]&)]

SpinorSquareBracket /: MakeBoxes[SpinorSquareBracket[a_, b_], StandardForm | TraditionalForm] := SpinorSquareBracketBox[ToBoxes[a], ToBoxes[b]]

SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sb" -> SpinorSquareBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"]]]


(* ::Subsection:: *)
(*SpinorRoundBracket*)


SpinorRoundBracket[a_, b_] /; a == b := 0
SpinorRoundBracket[a_, b_] /; \[Not]OrderedQ[{a, b}] := -SpinorRoundBracket[b, a]

SpinorRoundBracketBox[a_, b_] :=
    TemplateBox[{a,b}, "SpinorRoundBracket",
        DisplayFunction -> (RowBox[{"(",RowBox[{#1,",",#2}],")"}]&),
        InterpretationFunction -> (RowBox[{"SpinorRoundBracket","[",RowBox[{#1,",",#2}],"]"}]&)]

SpinorRoundBracket /: MakeBoxes[SpinorRoundBracket[a_, b_], StandardForm | TraditionalForm] := SpinorRoundBracketBox[ToBoxes[a], ToBoxes[b]]

SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates @ Append[InputAliases /. Options[EvaluationNotebook[], InputAliases]], "rb" -> SpinorRoundBracketBox["\[SelectionPlaceholder]", "\[Placeholder]"]]


(* ::Subsection:: *)
(*SpinorUndotted*)


SpinorUndottedBox[a_] :=
    TemplateBox[{a}, "SpinorUndotted",
        DisplayFunction -> (SubscriptBox["\[Lambda]",#]&),
        InterpretationFunction -> (RowBox[{"SpinorUndotted","[",#,"]"}]&)]

SpinorUndotted /: MakeBoxes[SpinorUndotted[a_], StandardForm | TraditionalForm] := SpinorUndottedBox[ToBoxes[a]]

SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "su" -> SpinorUndottedBox["\[SelectionPlaceholder]"]]]


(* ::Subsection:: *)
(*SpinorDotted*)


SpinorDottedBox[a_] :=
    TemplateBox[{a}, "SpinorDotted",
        DisplayFunction -> (SubscriptBox[OverscriptBox["\[Lambda]","~"],#]&),
        InterpretationFunction -> (RowBox[{"SpinorDotted","[",#,"]"}]&)]

SpinorDotted /: MakeBoxes[SpinorDotted[a_], StandardForm | TraditionalForm] := SpinorDottedBox[ToBoxes[a]]

SetOptions[EvaluationNotebook[],
    InputAliases -> DeleteDuplicates@Append[InputAliases /. Options[EvaluationNotebook[], InputAliases], "sd" -> SpinorDottedBox["\[SelectionPlaceholder]"]]]


(* ::Subsection:: *)
(*SpinorContract*)


(* NOTE: Since we are not using explicit spinor indices with a definite position (raised or lowered), the output brackets simply reflect the order of the input. *)

(* SpinorIndices *)
SpinorIndices[expr_] :=
    Module[{u, d, cr},
        cr = CoefficientRules[expr/.{SpinorUndotted[_] -> u, SpinorDotted[_] -> d}, {u, d}];
        If[Length[cr] == 1,
            Switch[cr[[1]][[1]],
                {1,0}, "Undotted",
                {0,1}, "Dotted",
                {1,1}, "UndottedDotted",
                {0,0}, "Scalar",
                _, "Invalid"],
            "Invalid"]
    ]

(* SpinorContractUndotted *)
SpinorContractUndotted[k1_. SpinorUndotted[u1_], k2_. SpinorUndotted[u2_]] := k1 k2 SpinorAngleBracket[u1, u2]
SpinorContractUndotted[expr1_Plus, expr2_] := (s \[Function] SpinorContractUndotted[s, expr2]) /@ expr1
SpinorContractUndotted[expr1_, expr2_Plus] := (s \[Function] SpinorContractUndotted[expr1, s]) /@ expr2

(* SpinorContractDotted *)
SpinorContractDotted[k1_. SpinorDotted[d1_], k2_. SpinorDotted[d2_]] := k1 k2 SpinorSquareBracket[d1, d2]
SpinorContractDotted[expr1_Plus, expr2_] := (s \[Function] SpinorContractDotted[s, expr2]) /@ expr1
SpinorContractDotted[expr1_, expr2_Plus] := (s \[Function] SpinorContractDotted[expr1, s]) /@ expr2

(* SpinorContract*)
(* NOTE: In case of bad indices a generic error message is displayed *)
SpinorContract[expr1_, expr2_] :=
    Module[{c, rule},
        Switch[{SpinorIndices[expr1], SpinorIndices[expr2]},
            {"UndottedDotted" , "UndottedDotted"}, rule = {c[k1_. SpinorUndotted[u1_] SpinorDotted[d1_], k2_. SpinorUndotted[u2_] SpinorDotted[d2_]] :> k1 k2 SpinorAngleBracket[u1, u2] SpinorSquareBracket[d1, d2]},
            {"Undotted" | "UndottedDotted", "Undotted" | "UndottedDotted"}, rule = {c[k1_. SpinorUndotted[u1_], k2_. SpinorUndotted[u2_]] :> k1 k2 SpinorAngleBracket[u1, u2]},
            {"Dotted" | "UndottedDotted", "Dotted" | "UndottedDotted"}, rule = {c[k1_. SpinorDotted[d1_], k2_. SpinorDotted[d2_]] :> k1 k2 SpinorSquareBracket[d1, d2]},
            _, Message[SpinorContract::interr]; Abort[]];
        c[expr1 // Expand, expr2 // Expand] //.
            {c[x_ + y_, z_] :> c[x, z] + c[y, z],
             c[x_, y_ + z_] :> c[x, y] + c[x, z]} /.
            rule
    ]


(* ::Subsection:: *)
(*Momenta*)


Momenta[expr_] := Cases[{expr}, HoldPattern[SpinorAngleBracket[a_, b_] | SpinorSquareBracket[a_, b_]] :> Sequence[a, b], \[Infinity]] // DeleteDuplicates


(* ::Subsection:: *)
(*SpinorReplace*)


SpinorReplaceHelper[R_][expr_, SpinorUndotted[u_] -> repl_] :=
    Module[{replR},
        If[SpinorIndices[repl] == "Undotted",
            replR = repl /. {SpinorUndotted[a_] :> SpinorUndotted[R[a]]},
            Message[SpinorReplace::interr]; Abort[]
        ];
        expr /.
                {HoldPattern[SpinorAngleBracket[u, a_]] :> SpinorContractUndotted[replR, SpinorUndotted[a]],
                 HoldPattern[SpinorAngleBracket[a_, u]] :> SpinorContractUndotted[SpinorUndotted[a], replR]}
    ]

SpinorReplaceHelper[R_][expr_, SpinorDotted[d_] -> repl_] :=
    Module[{replR},
        If[SpinorIndices[repl] == "Dotted",
            replR = repl /. {SpinorDotted[a_] :> SpinorDotted[R[a]]},
            Message[SpinorReplace::interr]; Abort[]
        ];
        expr /.
                {HoldPattern[SpinorSquareBracket[d, a_]] :> SpinorContractDotted[replR, SpinorDotted[a]],
                 HoldPattern[SpinorSquareBracket[a_, d]] :> SpinorContractDotted[SpinorDotted[a], replR]}
    ]

SpinorReplace[expr_, SpinorUndotted[u_] -> repl_] := SpinorReplaceHelper[Identity][expr, SpinorUndotted[u] -> repl]
SpinorReplace[expr_, SpinorDotted[d_] -> repl_] := SpinorReplaceHelper[Identity][expr, SpinorDotted[d] -> repl]

SpinorReplace[expr_, {}] := expr
SpinorReplace[expr_, l:{__Rule}] :=
    Module[{R},
        Fold[SpinorReplaceHelper[R], expr, l] /. {R -> Identity}
    ]

SpinorReplace[rules_][expr_] := SpinorReplace[expr, rules]


(* ::Subsection:: *)
(*SpinorWeight*)


SpinorWeight[expr_, a_] :=
    Module[{t, cr},
        cr = CoefficientRules[SpinorReplace[expr, {SpinorUndotted[a] -> t SpinorUndotted[a], SpinorDotted[a] -> t^(-1) SpinorDotted[a]}], t];
        If[Length[cr] == 1, cr[[1]][[1]][[1]], Message[SpinorWeight::interr]; Abort[]]
    ]

SpinorWeight[expr_] := Association[Table[m -> SpinorWeight[expr, m], {m, Momenta[expr]}]]


(* ::Subsection:: *)
(*MassDimension*)


MassDimension[expr_] :=
    Module[{t, cr},
        cr = CoefficientRules[expr /. {b : SpinorAngleBracket[__] :> t b, b : SpinorSquareBracket[__] :> t b}, t];
        If[Length[cr] == 1, cr[[1]][[1]][[1]], Message[MassDimension::interr]; Abort[]]
    ]


(* ::Subsection:: *)
(*MomentumSquare*)


(* NOTE: We are making a choice for the sign of the square brackets. We could introduce a global variable to choose between different conventions. *)
MomentumSquare[momenta_List] := Sum[SpinorAngleBracket[sub[[1]], sub[[2]]] SpinorSquareBracket[sub[[2]], sub[[1]]], {sub, Subsets[momenta, {2}]}]


(* ::Subsection:: *)
(*SpinorSimplify*)


(* Rules for Schouten simplification *)
SchoutenRules =
    {k_. SpinorAngleBracket[a_,c_] SpinorAngleBracket[b_,d_] - k_. SpinorAngleBracket[a_,d_] SpinorAngleBracket[b_,c_] :> k SpinorAngleBracket[a,b] SpinorAngleBracket[c,d],
     k_. SpinorAngleBracket[a_,b_] SpinorAngleBracket[c_,d_] + k_. SpinorAngleBracket[a_,d_] SpinorAngleBracket[b_,c_] :> k SpinorAngleBracket[a,c] SpinorAngleBracket[b,d],
     k_. SpinorAngleBracket[a_,c_] SpinorAngleBracket[b_,d_] - k_. SpinorAngleBracket[a_,b_] SpinorAngleBracket[c_,d_] :> k SpinorAngleBracket[a,d] SpinorAngleBracket[b,c],
     k_. SpinorSquareBracket[a_,c_] SpinorSquareBracket[b_,d_] - k_. SpinorSquareBracket[a_,d_] SpinorSquareBracket[b_,c_] :> k SpinorSquareBracket[a,b] SpinorSquareBracket[c,d],
     k_. SpinorSquareBracket[a_,b_] SpinorSquareBracket[c_,d_] + k_. SpinorSquareBracket[a_,d_] SpinorSquareBracket[b_,c_] :> k SpinorSquareBracket[a,c] SpinorSquareBracket[b,d],
     k_. SpinorSquareBracket[a_,c_] SpinorSquareBracket[b_,d_] - k_. SpinorSquareBracket[a_,b_] SpinorSquareBracket[c_,d_] :> k SpinorSquareBracket[a,d] SpinorSquareBracket[b,c]}

(* Generates all possibile Schouten identities associated with a given set of momenta *)
SchoutenIdentities[momenta_List] :=
    ({a,b,c,d} \[Function]
        Sequence@@
            {SpinorAngleBracket[a,b] SpinorAngleBracket[c,d] + SpinorAngleBracket[b,c] SpinorAngleBracket[a,d] + SpinorAngleBracket[c,a] SpinorAngleBracket[b,d] == 0,
             SpinorSquareBracket[a,b] SpinorSquareBracket[c,d] + SpinorSquareBracket[b,c] SpinorSquareBracket[a,d] + SpinorSquareBracket[c,a] SpinorSquareBracket[b,d] == 0})@@@
        Subsets[momenta,{4}]

(* Generates all unordered bipartitions of a given set where each term has fixed minimum cardinality *)
Bipartitions[set_List, minlength_Integer] := (Cases[#, _?OrderedQ]&) @ Table[{sub, Complement[set, sub]}, {sub, Subsets[set, {minlength, \[LeftFloor]Length[set]/2\[RightFloor]}]}]

(* Generates momentum conservation identities *)
MomentumConservationIdentities[{positive_List, negative_List}, extramomenta_List]["Contractions"] :=
    ({a, b} \[Function] Sum[SpinorAngleBracket[a, m] SpinorSquareBracket[b, m], {m, positive}] == Sum[SpinorAngleBracket[a, m] SpinorSquareBracket[b, m], {m, negative}]) @@@
        Tuples[Join[positive, negative, extramomenta] // DeleteDuplicates, 2]

MomentumConservationIdentities[momenta_List, extramomenta_List]["Contractions"] :=
    MomentumConservationIdentities[{momenta, {}}, extramomenta]["Contractions"]

MomentumConservationIdentities[{positive_List, negative_List}]["SquarePartitions"] :=
    Map[SpinorReplace[(m \[Function] (SpinorUndotted[m] -> -SpinorUndotted[m])) /@ negative],
        ({m1, m2} \[Function] MomentumSquare[m1] == MomentumSquare[m2]) @@@ Bipartitions[Join[positive, negative], 2],
            {2}]

MomentumConservationIdentities[momenta_List]["SquarePartitions"] := 
    MomentumConservationIdentities[{momenta, {}}]["SquarePartitions"]

MomentumConservationIdentities[{positive_List, negative_List}, extramomenta_List]["All"] :=
    Join[MomentumConservationIdentities[{positive, negative}, extramomenta]["Contractions"],
         MomentumConservationIdentities[{positive, negative}]["SquarePartitions"]]

MomentumConservationIdentities[momenta_List, extramomenta_List]["All"] :=
    Join[MomentumConservationIdentities[momenta, extramomenta]["Contractions"],
         MomentumConservationIdentities[momenta]["SquarePartitions"]]

(* Main function *)
(* NOTE: We are not performing any check on the list of input momenta. *)
SpinorSimplify[expr_, mlists__List] :=
	Module[{mexpr = Momenta[expr]},
        Simplify[expr,
            Assumptions -> Join[SchoutenIdentities[mexpr], MomentumConservationIdentities[#, mexpr]["All"]& /@ {mlists} //Flatten],
            TransformationFunctions -> {Automatic, (e \[Function] e /. SchoutenRules)}]
    ]

SpinorSimplify[expr_] :=
    Simplify[expr,
        Assumptions -> SchoutenIdentities[Momenta[expr]],
        TransformationFunctions -> {Automatic, (e \[Function] e /. SchoutenRules)}]


(* ::Subsection:: *)
(*ParkeTaylor*)


ParkeTaylor["Angle"][momenta_List] := Times @@ (SpinorAngleBracket @@@ Partition[Append[momenta, momenta[[1]]], 2, 1])
ParkeTaylor["Square"][momenta_List] := Times @@ (SpinorSquareBracket @@@ Partition[Append[momenta, momenta[[1]]], 2, 1])
ParkeTaylor["Round"][momenta_List] := Times @@ (SpinorRoundBracket @@@ Partition[Append[momenta, momenta[[1]]], 2, 1])


End[]


(* ::Section:: *)
(*Attributes*)


SetAttributes[
    {SpinorAngleBracket,
     SpinorAngleBracketBox,
     SpinorSquareBracket,
     SpinorSquareBracketBox,
     SpinorUndottedBox,
     SpinorUndotted,
     SpinorDottedBox,
     SpinorDotted,
     SpinorContract,
     SpinorReplace,
     SpinorWeight,
     MassDimension},
    Protected
]


EndPackage[]
