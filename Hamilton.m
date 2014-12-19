(* ::Package:: *)

BeginPackage["`Hamilton`"]

Hamilton::usage = "Hamilton[objective, constraints] returns a fully formatted version of the maximization program and its associated first order conditions";


Begin["`Private`"]

MakeBoxes[alignedEquations[eqs_],fmt_]:=GridBox[Map[ToBoxes,eqs/.{rhs_==lhs_->{rhs,"=" ,lhs},rhs_<=lhs_->{rhs,"\[LessEqual]" ,lhs},rhs_>=lhs_->{rhs,"\[GreaterEqual]" ,lhs}},{2}],GridBoxAlignment->{"Columns"->{Right,Center,Left}}] ;

MakeBoxes[alignedMultiple[eqs_],fmt_]:=GridBox[Map[ToBoxes,eqs/.{{rhs_==lhs_,r__}->{rhs,"=" ,lhs,r},{rhs_<=lhs_,r__}->{rhs,"\[LessEqual]" ,lhs,r},{rhs_>=lhs_,r__}->{rhs,"\[GreaterEqual]" ,lhs,r}},{2}],GridBoxAlignment->{"Columns"->{Right,Center,Left,Center}}] ;

MakeBoxes[bracket[obj_],fmt_]:=StyleBox[RowBox[{"{",obj~ToBoxes~fmt}],SpanMaxSize->Infinity];

greeks =Complement[CharacterRange["\[Alpha]", "\[Omega]"],{"\[Delta]","\[CurlyEpsilon]","\[Zeta]","\[Theta]","\[Kappa]","\[Iota]","\[Omicron]","\[Pi]","\[Rho]","\[Sigma]","\[FinalSigma]","\[Tau]","\[Upsilon]","\[Omega]"}];

ToTimeFunction[lst_]:=Replace[lst,(x_)->x[t],2];

RuleToEquation:=(lhs_->rhs_)->(lhs==rhs);

Hamiltonian[obj0_,eqs0_]:=Module[{obj=obj0,eqs=eqs0},
multipliers=ToTimeFunction[Take[greeks,{1,Length[eqs]}]];
vars=DeleteDuplicates[Cases[Prepend[eqs,obj],Except[_'[t],f_[t]],10]];
states=Cases[eqs,f_'[t]==rhs_->f[t]];stateMultipliers=Take[multipliers,{1,Length[states]}];
stateEqs=Cases[eqs,_'[t]==_]/.f_'[t]==rhs_->rhs;
controls=Complement[vars, states];
controlMultipliers=Take[multipliers,{Length[states]+1,-1}];
controlEqs=Cases[eqs,Except[_'[t]==_]]/.{lhs_==rhs_->lhs-rhs,lhs_<rhs_->rhs-lhs,lhs_<=rhs_->rhs-lhs,lhs_>rhs_->lhs-rhs,lhs_>=rhs_->lhs-rhs};
H=Exp[-\[Rho] t](obj+Fold[Plus,0,Join[Apply[Function[{x,y},x y],#]&/@Transpose[{stateMultipliers,stateEqs}],Apply[Function[{x,y},x y],#]&/@Transpose[{controlMultipliers,controlEqs}]]]);
multipliersInOrder=ConstantArray[0,Length[eqs]];
multipliersInOrder[[Flatten[Position[eqs,_'[t]==_,{1},Heads->False]]]]=stateMultipliers(*/.f_[t]\[Rule](f)*);
multipliersInOrder[[Flatten[Position[eqs,Except[_'[t]==_],{1},Heads->False]]]]=controlMultipliers(*/.f_[t]\[Rule](f)*);
{H, controls, stateMultipliers,states,multipliersInOrder}];

HamiltonianFOC[H0_,controls0_,costates0_,states0_]:=Module[{H=H0,controls=controls0,costates=costates0,states=states0},Flatten[{Simplify[Solve[D[H,#]==0][[1,1]]]&/@controls,Apply[Function[{x,y},Simplify[Solve[D[H,x]==-D[Exp[-\[Rho] t]y,t]][[1,1]]]],#]&/@Transpose[{states,costates}](*Apply[Function[{x,y},Simplify[Solve[D[H,x]\[Equal]-D[Exp[-\[Rho] t]y,t]][[1,1]]]],#]&/@Transpose[{states,costates}]*)}]/.RuleToEquation];

Hamilton[obj0_,eqs0_]:=Module[{obj=obj0,eqs=eqs0},
h=Hamiltonian[obj,eqs];
foc=HamiltonianFOC@@Take[h, {1, 4}];
(*{h,foc}*)
expObj=Exp[-\[Rho]t]obj;
multipliersInOrder=h[[5]]/.f_[t]:>"("~~ToString[f]~~")";
Style[Grid[{
{Row[{max,Integrate[expObj,{t,0,Infinity}]}],SpanFromLeft},
{Spacer[{10,10}],SpanFromLeft},
{Item[Style["s.t.",Italic],Alignment->Top],Transpose[{eqs,multipliersInOrder}]//alignedMultiple//bracket},
{SpanFromAbove,Spacer[{10,10}]},
{Item[Style["FOC",Italic],Alignment->Top],foc//alignedEquations//bracket}
}, Alignment->{{Right, Left},Automatic,{{1,1}->Center}}],Larger]
];

End[]

EndPackage[]



