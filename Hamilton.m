(* ::Package:: *)

(* ::Text:: *)
(*Hamilton Package by A. Saussay (2015)*)
(*Released under the MIT license.*)


BeginPackage["`Hamilton`"]

Hamilton::usage="Hamilton[objective, constraints, Output -> \"Full\", Multipliers -> {}] automatically derives the first order conditions for a standard economics continuous dynamic optimization problem.";
t::usage="Time variable used by the Hamilton package";
i::usage="Index that can be used as a subscript in the optimization problem";
e::usage="Exponent used by the Hamilton package";
\[Rho]::usage="Discount rate used by the Hamilton package";
\[Rho]t::usage="Discount factor used by the Hamilton package";
max::usage="Maximization operator used by the Hamilton package";

Begin["`Private`"]

MakeBoxes[alignedEquations[eqs_],fmt_]:=GridBox[Map[ToBoxes,eqs/.{rhs_==lhs_->{rhs,"=" ,lhs},rhs_<=lhs_->{rhs,"\[LessEqual]" ,lhs},rhs_>=lhs_->{rhs,"\[GreaterEqual]" ,lhs}},{2}],GridBoxAlignment->{"Columns"->{Right,Center,Left}}] ;

MakeBoxes[alignedMultiple[eqs_],fmt_]:=GridBox[Map[ToBoxes,eqs/.{{rhs_==lhs_,r__}->{rhs,"=" ,lhs,r},{rhs_<=lhs_,r__}->{rhs,"\[LessEqual]" ,lhs,r},{rhs_>=lhs_,r__}->{rhs,"\[GreaterEqual]" ,lhs,r}},{2}],GridBoxAlignment->{"Columns"->{Right,Center,Left,Center}}] ;

MakeBoxes[bracket[obj_],fmt_]:=StyleBox[RowBox[{"{",obj~ToBoxes~fmt}],SpanMaxSize->Infinity];

buildCSC[eqs0_,multipliers0_]:=Module[{eqs=eqs0,multipliers=multipliers0},
mixed=Cases[Transpose[{eqs,multipliers}],{_>= _,_}|{_<=_,_}]/.{{l_>=0,m_}-> {l,"\[GreaterEqual]","0,",m,"\[GreaterEqual]",0,"and",m l,"=","0"},
{l_>=r_,m_}->{l-r,"\[GreaterEqual]","0,",m,"\[GreaterEqual]",0,"and",m(l-r),"=","0"},
{l_<=r_,m_}->{r-l,"\[GreaterEqual]","0,",m,"\[GreaterEqual]",0,"and",m(r-l),"=","0"}};
GridBox[Map[ToBoxes,mixed,{2}],
GridBoxAlignment->{"Columns"->{Right,Center,Left,Right,Center,Left,Center,Right,Center,Left}}]//DisplayForm];

buildTC[states_,costates_]:=GridBox[Map[ToBoxes,Transpose[{states,costates}]/.{s_,cs_}->{HoldForm[Limit[e^(-\[Rho]t)cs s,t->Infinity]]//TraditionalForm,"=",0},{2}],GridBoxAlignment->{"Columns"->{Left,Center,Left}}]//DisplayForm;

greeks =Complement[CharacterRange["\[Alpha]", "\[Omega]"],{"\[Delta]","\[CurlyEpsilon]","\[Zeta]","\[Theta]","\[Kappa]","\[Iota]","\[Omicron]","\[Pi]","\[Rho]","\[Sigma]","\[FinalSigma]","\[Tau]","\[Upsilon]","\[Omega]"}];

ToTimeFunction[lst0_]:=Module[{lst=lst0},Function[s,s/.x_->x[t]]/@lst];

RuleToEquation:=(lhs_->rhs_)->(lhs==rhs);

buildProd[eqs_,multipliers_]:=MapAt[Sum[#,i]&, Apply[Function[{x,y},x y],#]&/@Transpose[{multipliers,eqs}], Position[multipliers,Subscript[_,_][t]]];

Hamiltonian[obj0_,eqs0_,multipliers0_,controls0_,states0_]:=
Module[{obj=obj0, eqs=eqs0, tmpMultipliers=multipliers0, tmpControls=controls0, tmpStates=states0},
multipliers=ToTimeFunction[If[Length[tmpMultipliers]==0,
Take[greeks,{1,Length[eqs]}],
tmpMultipliers]];
vars=DeleteDuplicates[Cases[Prepend[eqs,obj],Except[_'[t],f_[t]],10]];

states=If[Length[tmpStates]==0,Cases[eqs,f_'[t]==rhs_->f[t]],tmpStates];
stateMultipliers=multipliers[[Sort[Flatten[Function[Position[eqs,#]]/@ Cases[eqs,f_'[t]==_]]]]];
stateEqs=Cases[eqs,_'[t]==_]/.f_'[t]==rhs_->rhs;


controls=If[Length[tmpControls]==0,Complement[vars, states],tmpControls];
controlMultipliers=multipliers[[Sort[Flatten[Function[Position[eqs,#]]/@Cases[eqs,Except[_'[t]==_]]]]]];
controlEqs=Cases[eqs,Except[_'[t]==_]]/.{lhs_==rhs_->lhs-rhs,lhs_<rhs_->rhs-lhs,lhs_<=rhs_->rhs-lhs,lhs_>rhs_->lhs-rhs,lhs_>=rhs_->lhs-rhs};

H=Exp[-\[Rho] t](obj+Fold[Plus,0,Join[buildProd[stateEqs,stateMultipliers],buildProd[controlEqs,controlMultipliers]]]);

multipliersInOrder=ConstantArray[0,Length[eqs]];
multipliersInOrder[[Flatten[Position[eqs,_'[t]==_,{1},Heads->False]]]]=stateMultipliers(*/.f_[t]\[Rule](f)*);
multipliersInOrder[[Flatten[Position[eqs,Except[_'[t]==_],{1},Heads->False]]]]=controlMultipliers(*/.f_[t]\[Rule](f)*);
{H, controls, stateMultipliers,states,multipliersInOrder}];

(*Hackish solution to the differentiation of a finite symbolic sum
  Transform the symbolic sum into an explicit sum with 100 elements,
  differentiate wrt element 42, then identify the various terms to get back 
  to the symbolic representation.*)
DSum[f0_,y0_]:=Module[{f=f0,y=y0},
expansionRule:=(Sum[e_,_]:>Fold[Plus,0,(e/.i->#)&/@Range[100]]);
expandedF=f/.expansionRule;
sums=Cases[H,Sum[_,_],{1,Infinity}];
expandedSums=sums/.expansionRule;
expandedDiff =D[expandedF,y/.Subscript[e_,i]->Subscript[e,42]];
(expandedDiff/.(Transpose[{expandedSums,sums}]/.{lhs_,rhs_}:>lhs->rhs))/.(Subscript[e_,42]->Subscript[e,i])
];

HamiltonianFOC[H0_,controls0_,costates0_,states0_]:=Module[{H=H0,controls=controls0,costates=costates0,states=states0},
Flatten[{
FullSimplify[DSum[H,#]==0,t>=0]&/@controls,
Apply[Function[{x,y},FullSimplify[DSum[H,x]==-D[Exp[-\[Rho] t]y,t],t>=0]],#]&/@Transpose[{states,costates}]
}]/.RuleToEquation];

dotTimeDerivative:={Subscript[e_,j_]'[t]->Subscript[OverDot[e],j][t],f_'[t]->OverDot[f[t]]};
timeSubscript:={Subscript[e_,j_][t]->Subscript[e,Row[{j,",",t}]],f_[t]->Subscript[f,t]};

Hamilton::badoutput="`1` is not a valid output option, returning Full output.";
Hamilton::badmultipliers="Incorrect number of multipliers provided. `1` expected, `2` received. Reverting to default choice of multipliers";

Hamilton[obj0_,eqs0_,OptionsPattern[{Output->"Full",Multipliers->{},Controls->{},States->{}}]]:=
Module[{obj=obj0,eqs=eqs0,
format=OptionValue[Output],
multipliers=OptionValue[Multipliers],
controls=OptionValue[Controls],
states=OptionValue[States]},
If[Length[multipliers]>0&&Length[multipliers]!=Length[eqs],Message[Hamilton::badmultipliers,Length[eqs],Length[multipliers]]];
(*t0=Cases[u[c[t]],t,{0,Infinity}][[1]];*)
h=Hamiltonian[obj,eqs,multipliers];
foc=HamiltonianFOC@@Take[h, {1, 4}];
expObj=Exp[-\[Rho]t]obj;
multipliersInOrder=h[[5]]/.f_[t]:>Row[{"(",f[t],")"}]/.dotTimeDerivative/.timeSubscript;
full=Style[Grid[{
{Row[{max,Integrate[expObj,{t,0,Infinity}]}],SpanFromLeft},
{Item[Style["s.t.",Italic],Alignment->Top],Transpose[{eqs/.dotTimeDerivative/.timeSubscript,multipliersInOrder}]//alignedMultiple//bracket},
{SpanFromAbove,Spacer[{10,10}]},
{Item[Style["FOC",Italic],Alignment->Top],foc/.dotTimeDerivative/.timeSubscript//alignedEquations//bracket},
{SpanFromAbove,Spacer[{10,10}]},
{Item[Style["TC",Italic],Alignment->Top],buildTC[h[[4]]/.dotTimeDerivative/.timeSubscript,h[[3]]/.dotTimeDerivative/.timeSubscript]//bracket},
{SpanFromAbove,Spacer[{10,10}]},
{Item[Style["CSC",Italic],Alignment->Top],buildCSC[eqs/.dotTimeDerivative/.timeSubscript,h[[5]]/.dotTimeDerivative/.timeSubscript]//bracket}
}, Alignment->{{Right, Left},Automatic,{{1,1}->Center}}],Larger];
Switch[format, 
"Hamiltonian", h[[1]],
"Controls", h[[2]],
"States", h[[4]],
"FOC", foc,
"Full", full,
_, Message[Hamilton::badoutput, format];full]
];
End[]

EndPackage[]






