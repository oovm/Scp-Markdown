(* ::Package:: *)



ScpSpecial::usage="";
Begin["`Special`"];

ScpSpecial[OptionsPattern[]]:=Block[
    {from,case,get},
    from={ScpFromD,ScpFromArc,ScpFromEx,ScpFromJ};
    case={$ScpCaseD,$ScpCaseArc,$ScpCaseEx,$ScpCaseJ};
    get=Flatten[Activate@Thread[Inactive[Map][from,case]]];
    Select[Join[get,$ScpCaseAdd],StringContainsQ[#["Number"],"-"~~__~~"-"]&]
]




(* ::Section:: *)
(*Scp-J*)


$ScpCaseJ = {
    "//scp-wiki-cn.wikidot.com/joke-scps",
    "//scp-wiki-cn.wikidot.com/joke-scps-cn"
};
ScpFromJ[link_] := Cases[
    URLExecute[link, {"HTML", "XMLObject"}],
    XMLElement["li", {},
        {XMLElement["a", {"shape" -> "rect", "href" -> url_}, {num_}], name_}
    ] :> <|
        "Url" -> StringJoin["//scp-wiki-cn.wikidot.com", url],
        "Number" -> num,
        "Name" -> StringDelete[name, " - "]
    |>,
    Infinity
];



(* ::Section:: *)
(*Scp-D*)


$ScpCaseD = {
    "//scp-wiki-cn.wikidot.com/decommissioned-scps-arc",
    "//scp-wiki-cn.wikidot.com/scp-removed"
};
ScpFromD[link_] := Cases[FirstCase[
    URLExecute[link, {"HTML", "XMLObject"}],
    XMLElement["div", {"class" -> "content-panel standalone series"}, __],
    0, Infinity
],
    XMLElement["li", {},
        {XMLElement["a", {"shape" -> "rect", "href" -> url_}, ___], name_}
    ] :> <|
        "Url" -> StringJoin["//scp-wiki-cn.wikidot.com", url],
        "Number" -> StringJoin["SCP-", StringDelete[StringCases[url, "scp-" ~~ x__ -> x], {"-d"}], "-D"],
        "Name" -> StringDelete[name, {" - ", "\:ff08\:65e7\:ff09"}]
    |>,
    Infinity
];



(* ::Section:: *)
(*Scp-Ex*)


$ScpCaseEx = {
    "//scp-wiki-cn.wikidot.com/scp-ex"
};
ScpFromEx[link_] := Cases[
    URLExecute[link, {"HTML", "XMLObject"}],
    XMLElement["li", {},
        {XMLElement["a", {"shape" -> "rect", "href" -> url_}, {num_}], name_}
    ] :> <|
        "Url" -> StringJoin["//scp-wiki-cn.wikidot.com", url],
        "Number" -> num,
        "Name" -> StringDelete[name, " - "]
    |>,
    Infinity
];


(* ::Section:: *)
(*Scp-Arc*)


$ScpCaseArc = {
    "//scp-wiki-cn.wikidot.com/archived-scps"
};
ScpFromArc[link_] := Cases[
    URLExecute[link, {"HTML", "XMLObject"}],
    XMLElement["li", {},
        {XMLElement["a", {"shape" -> "rect", "href" -> url_}, {num_}], name_}
    ] :> <|
        "Url" -> StringJoin["//scp-wiki-cn.wikidot.com", url],
        "Number" -> num,
        "Name" -> StringDelete[name, " - "]
    |>,
    Infinity
];


(* ::CodeText:: *)
(*all = Quiet@Join[ScpGetCN[], ScpGetOther[], $ScpAdd]*)
(*Select[all, StringTake[#["Number"], -1] === "J" &]*)

$ScpCaseAdd = {
    <|"Url"->"//scp-wiki-cn.wikidot.com/scp-one-half-jp-j","Number"->"SCP-1/2-JP-J","Name"->"\:534a\:4ef7"|>,
    <|"Url"->"//scp-wiki-cn.wikidot.com/scp-36-6-jp-j","Number"->"SCP-36.6-JP-J","Name"->"\:4e0d\:53ef\:89c1\:7684\:6076\:9b54"|>,
    <|"Url"->"//scp-wiki-cn.wikidot.com/scp-666-jp-j","Number"->"SCP-666-JP-J","Name"->"\:7a76\:6781\:6050\:6016\:7269\:4f53"|>,
    <|"Url"->"//scp-wiki-cn.wikidot.com/scp-710-jp-j","Number"->"SCP-710-JP-J","Name"->"\:57fa\:91d1\:4f1a\:795e\:62f3"|>,
    <|"Url"->"//scp-wiki-cn.wikidot.com/scpaaaaaaaaaaaaaaaaaa-jp-j","Number"->"SCP-\:54e6\:5594\:5594\:963f\:963f\:963f\:963f\:963f\:963f\:963f\:963f-JP-J","Name"->""|>,
    <|"Url"->"//scp-wiki-cn.wikidot.com/scp-017-fr-j","Number"->"SCP-017-FR-J","Name"->"\:9752\:9752\:9152\:539f"|>,
    <|"Url"->"//scp-wiki-cn.wikidot.com/scp-008-de-j","Number"->"SCP-008-DE-J","Name"->"\:7fd4\:ff1f\:ff01"|>
};


(* ::Chapter::Closed:: *)
(*End*)


End[]
