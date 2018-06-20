(* ::Package:: *)

Begin["`Special`"];




(* ::Section:: *)
(*ScpD*)


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
(*ScpArc*)


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


(* ::Section:: *)
(*ScpEx*)


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
(*ScpJ*)


$ScpCaseJ = {
    "//scp-wiki-cn.wikidot.com/joke-scps",
    "//scp-wiki-cn.wikidot.com/joke-scps-cn"
};
$ScpCaseSp = {};
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



End[]