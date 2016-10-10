(* ::Package:: *)

(* ::Title:: *)
(*Crystal structure package*)


(* ::Subsection:: *)
(*Shapes*)


p1:= Polygon[{{1,0,0.5},{1,1,0.5},{1,0.9,1.5},{1,.2,0.5}, {1.3, 0.7, 1}, {1.3,0.3,1}, {1, 0.3, 1}}];


pot:=Module[{z2, x1, x2},
z2 = 0.75;
x1 = 0.6;
x2 = 0.05;
{
Cylinder[{{1, 1, 0.5},{1, 1, z2 }}, 0.2]
,
Polygon[{{0.6, 0.4,z2 }, {0.9,0.85,z2 }, {0.9, 0.75, z2 }, {0.7, 0.4, z2 }}]
,
Black
,
Thickness[0.01]
,
BezierCurve[{{1,1,z2}, {1.2, 1.3, z2+0.1},{.6,1,z2+0.2},{1,1.3,z2+0.3}}]
(*,
BezierCurve[{{.94,.12,z2}, {.8, .1, z2+0.07},{1.3,.12,z2+0.15},{1,.3,z2+0.4}, {0.8, .2, z2+0.6}}]*)
}
]


(* ::Subsection:: *)
(*Functions*)


transSeq[p1_,i_, r_, pr_, bg_]:=Module[{t,p,l},
t = 0.01;
l =  Line[{{0,0,-1}, {0,0,1}}];
p = Sphere[{0,0,0}, 0.05];
Join[
Graphics3D[{bg, GeometricTransformation[p1, RotationMatrix[#, {0,0,1}]],
Thickness[t],l, p}, PlotRange->pr, Boxed->False, ImageSize->Large, PreserveImageOptions->False]&/@Range[0, 2*Pi/i, 2*Pi/60]
,
If[r, 
Graphics3D[{bg, GeometricTransformation[p1, RotationMatrix[2*Pi/i, {0,0,1}]*#],
Thickness[t],l, p}, PlotRange->pr, Boxed->False, ImageSize->Large, PreserveImageOptions->False]&/@(-Range[0,2,0.08]+1),Graphics3D[{bg, GeometricTransformation[p1, RotationMatrix[2*Pi/i, {0,0,1}]],
Thickness[t], l, p}, PlotRange->pr, Boxed->False, ImageSize->Large, PreserveImageOptions->False]&/@ConstantArray[0, 10]]
,
If[r,Graphics3D[{bg, GeometricTransformation[p1, RotationMatrix[2*Pi/i, {0,0,1}]*-1],
Thickness[t],l, p}, PlotRange->pr, Boxed->False, ImageSize->Large, PreserveImageOptions->False]&/@(ConstantArray[0,20]),{}]
]
];


transFin[im_, i_, r_]:=Module[{},
GeometricTransformation[im, RotationMatrix[2*Pi/i, {0,0,1}]*If[r, -1, 1]]
];


(* ::Subsection:: *)
(*Interfaces*)


crystalStructure:=Manipulate[

r1max = Switch[cst, 
	1, 0.5*Min[as, bs, cs], 
	2, If[s2Bool, 0.5*Min[as, bs, cs], Min[.25*Sqrt[as^2+ bs^2+ cs^2], .5*Min[as, bs, cs]]], 
	3, If[s2Bool, 0.5*Min[as, bs, cs], Min[.25*Sqrt[Min[as^2+ bs^2, bs^2+ cs^2,as^2+ cs^2]], 0.5*Min[as, bs, cs]]]
];
r2max = Switch[cst
	, 1, 0.5
	, 2, If[s2Bool, Min[0.5*(Sqrt[as^2+ bs^2+ cs^2]-2*r1max*r), 0.5*Min[as,bs,cs]],r1max] 
	, 3, If[s2Bool, Min[0.5*(Sqrt[Min[as^2+ bs^2,bs^2+ cs^2,as^2+ cs^2]]-2*r1max*r),0.25*(Sqrt[Min[as^2+ bs^2,bs^2+ cs^2,as^2+ cs^2]])], r1max]
];
l  ={as, bs, cs}*#&/@ {{0,0.5,0.5},{0.5,0,0.5},{0.5,0.5,0}};
extra = Sqrt[Total[cp*(#^2&/@{r*r1max, r*r1max, r*r1max})]];
tl = Total[cp*{(as*ar),(bs*br),(cs*cr)}]+2*extra;
distance = -d*tl+extra;

(*------find locations--------*)
sphlocs = Flatten[Table[{i*as,j*bs,k*cs}, {i, 0,ar},{j,0,br},{k,0,cr}],1];
sph2locs = Switch[cst
			,2, Flatten[Table[{as, bs, cs}*({.5,.5,.5}+{i,j,k}),  {i, 0, ar-1}, {j,0,br-1}, {k,0,cr-1}],1]
			,3, Flatten[Table[l[[#]]+{as*i,bs*j,cs*k}, {i, 0, ar-If[#==1,0,1]}, {j,0,br-If[#==2,0,1]}, {k,0,cr-If[#==3,0,1]}]&/@{1,2,3},3]
];
cublocs = Flatten[Table[{{0,0,0}+{as*i,bs*j,cs*k}, {as, bs, cs}*({1,1,1}+{i,j,k})}, {i, 0, ar-1}, {j,0,br-1}, {k,0,cr-1}],2];

(*------Begin graphics--------*)
Row[{
(*--------crystal--------*)
Graphics3D[{
	Opacity[sOp]
	,
	sCol
	,
	Sphere[#, r*r1max]&/@sphlocs
	,
If[cst>1, {If[s2Bool, s2Col],If[s2Bool, Opacity[s2Op]],Sphere[#, If[s2Bool, r2*r2max, r*r1max]]&/@sph2locs}]
	,
	If[cBool,{cCol,Opacity[cOp], Cuboid/@cublocs}]
,
Black
,
Arrow[{{0,0,0}, #}]&/@{{as,0,0},{0,bs,0},{0,0,cs}}
, 
Text[Style[#[[1]], Large], #[[2]]]&/@{{StringJoin[{"a, ", ToString[as]}], {0.5*as, -0.1,-0.1}}, {StringJoin[{"b, ", ToString[bs]}], {-0.1,0.5*bs,-0.1}},{StringJoin[{"c, ", ToString[cs]}], {-0.1,-0.1,0.5*cs}}}

}
,Boxed->False
,ImageSize->Large
,ClipPlanes->{Join[{1/as, 1/bs, 1/cs}*cp, {distance}]}
]
,
(*--------stats----------*)
Grid[{
{"a", as}, 
{"b", bs},
 {"c", cs},
{"r1", r},
{"r2",r2},  
{"Max r1",r1max}, 
{"Max r2", r2max},
{"Extra", extra},
{"Total length", tl},
{"Distance", distance}
}]
}]

(*----------options-----------*)
,{{cst, 1, "Crystal Structure"}, {1->"Simple Cubic", 2->"Body centered cubic", 3->"Face centered cubic"}}
,Delimiter
,{{ar, 1, "a repeats"}, 1,5,1}
,{{br, 1, "b repeats"}, 1,5,1}
,{{cr, 1, "c repeats"}, 1,5,1}
,{{as, 1, "a spacing"}, 1,5}
,{{bs, 1, "b spacing"}, 1,5}
,{{cs, 1, "c spacing"}, 1,5}
,Delimiter
,{{cBool, False, "Show unit cell"}, {True, False}}
,{{cCol, Pink, "Unit cell color"}, White}
, {{cOp, 0.2, "Unit cell opacity"}, 0.1, 1}
,Delimiter
,{{sCol, Purple, "Sphere color"}, White}
, {{sOp,1, "Sphere opacity"}, 0.1, 1}
, {{r, 0.5, "Sphere radius (% max)"}, 0.1, 1}
,Delimiter
,{{s2Bool, False, "Two types of spheres"}, {True, False}}
, {{s2Col, Yellow, "Sphere 2 color"}, White}
, {{s2Op,1, "Sphere opacity"}, 0.1, 1}
, {{r2, 0.5, "Sphere radius (% max)"}, 0.1, 1}
,Delimiter
,{{cp, {0,1,0}, "Cross-section"}, {{1,0,0}, {0,1,0}, {0,0,1}, {0,1,1}, {1,0,1}, {1,1,0}, {1,1,1}}}
,{{d, 0, "Clip %"}, 0, 1}
,ControlPlacement->Left
]


symmetryInterface:=Module[{},
Manipulate[
l = 1.5;
plane = Switch[mp
,{0,0,1}, Polygon[{{l, l, 0}, {-l, l, 0}, {-l, -l, 0},{l,-l, 0}}]
,{0,1,0}, Polygon[{{l, 0,l}, {-l, 0,l}, {-l, 0,-l},{l,0,-l}}]
,{1,0,0}, Polygon[{{0,l, l}, {0,-l, l}, {0,-l, -l},{0,l,-l}}]];
fullIm = FoldList[GeometricTransformation[#, RotationMatrix[2*Pi/i, {0,0,1}]*If[r,-1, 1]]&, im, Range[1, If[i==3 && r, 6, i]]];
Graphics3D[{
Pink
,
fullIm
,
Purple
,
If[m,GeometricTransformation[fullIm, ReflectionMatrix[mp]]]
,
Black
,
Thickness[0.01], Line[{{0,0,-1}, {0,0,1}}]
,
Sphere[{0,0,0}, 0.02]
,
Opacity[0.2]
,
If[m, plane]
},
Boxed->False]

,{{i,1, "Symmetry"}, {1,2,3,4,6}}
,{{r, False, "Rotoinversion"}, {True, False}}
,{{m, False, "Mirror"}, {True, False}}
,{{mp, {0,0,1}, "Mirror plane"}, {{0,0,1}, {0,1,0}, {1,0,0}}}
,{{im, pot, "Image"}, {p1->"polygon", pot->"pot"}}]]


rotAnim:=Module[{},
pr = {{-1.5,1.5},{-1.5,1.5},{-1.5,1.5}};
Manipulate[
posList = FoldList[transFin[#, i, r]&, im, Range[1, If[i==3 && r, 5, i-1]]];
ListAnimate[Flatten[transSeq[posList[[#]],i, r, pr, posList[[1;;#]]]&/@Range[1, Length[posList]],1]]
,{{i, 1, "Symmetry"}, {1,2,3,4,6}}
,{{r, False, "Rotoinversion"}, {True, False}}
,{{im, pot, "Image"}, {p1->"polygon", pot->"pot"}}]
]
