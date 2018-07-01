$SavePath::usage = "";
ScpGetContent::usage = "";
ScpGetConfig::usage = "";
ScpGetMarkdown::usage = "";

Begin["`Core`"];
$SavePath = FileNameJoin[{$UserBaseDirectory, "ApplicationData", "ScpMarkdown"}];
ScpGetConfig[] := Module[
    {},
    If[!FileExistsQ@#, CreateDirectory[#]]& /@ {
        FileNameJoin[{$SavePath, "Main"}],
        FileNameJoin[{$SavePath, "Branch"}],
        FileNameJoin[{$SavePath, "Special"}]
    };
    Export[FileNameJoin[{$SavePath, "main.config.json"}], ScpMain[]];
    Export[FileNameJoin[{$SavePath, "branch.config.json"}], ScpBranch[]];
    Export[FileNameJoin[{$SavePath, "special.config.json"}], ScpSpecial[]];
];


ScpGetContent[url_] := Module[
    {
        get = URLExecute[url, {"HTML", "XMLObject"}],
        rank0, rank1, rank2, rank3, head, md
    },
    head = Cases[get, XMLElement["div", {"id" -> "page-title"}, {h_}] :> Sequence["# ", StringDelete[h, {" ", "\n"}]], Infinity];
    rank0 = FirstCase[get, XMLElement["div", {"id" -> "page-content"}, a__] :> a, {"PhraseError"}, Infinity];
    rank1 = rank0 //. {
        XMLElement["p", {}, {p__}] :> {p, "\n"},
        XMLElement["div", _, {v_}] :> v
    };
    rank2 = rank1 //. {
        XMLElement["h1", _, {h_}] :> Sequence["## ", h, "\n"],
        XMLElement["h2", _, {h_}] :> Sequence["### ", h, "\n"],
        XMLElement["h3", _, {h_}] :> Sequence["#### ", h, "\n"],
        XMLElement["h4", _, {h_}] :> Sequence["##### ", h, "\n"],
        XMLElement["h5", _, {h_}] :> Sequence["###### ", h, "\n"],
        XMLElement["a", {"shape" -> "rect", "href" -> h_}, {m_}] :> {"[", m, "](", StringJoin["//scp-wiki-cn.wikidot.com", h], ")"},
        XMLElement["ul", {}, ul_] :> ul,
        XMLElement["blockquote", {}, q_] :> Riffle[q, "> "]
    };
    rank3 = rank2 //. {
        XMLElement["br", ___] :> Nothing,
        XMLElement["img", ___] :> Nothing,
        XMLElement["sub", ___] :> Nothing,
        XMLElement["hr", ___] :> "\n---\n",
        XMLElement["em", {}, {s_}] :> {"*", s, "* "},
        XMLElement["strong", {}, {s_}] :> {"**", s, "** "},
        XMLElement["li", {}, {li_}] :> {"- ", li, "\n"},
        XMLElement["span", _, {s_}] :> s
    };
    md = rank3 /. XMLElement[xml__] :> ExportString[XMLElement@xml, "XML"];
    Flatten[Join[head, md]]
];


$win = Thread[{"/", "\\", ":", "*", "?", "\"", "<", ">", "|"} -> " "];
ScpDownload[asc_, save_] := Module[
    {name, text},
    name = StringReplace[StringJoin[asc["Number"], " ", asc["Name"], ".md"], $win];
    If[FileExistsQ@FileNameJoin[{save, name}], Echo[Text@name, "Skip: "];Return[FileNameJoin[{save, name}]]];
    text = If[
        StringQ[asc["Url"]],
        ScpGetContent[asc["Url"]],
        Flatten@Map[ScpGetContent, asc["Url"]]
    ];
    Export[FileNameJoin[{save, name}], StringJoin@text, "Text"]
];

ScpGetMarkdown[file_] := Module[
    {save, records},
    save = FileNameJoin[{$SavePath, First@StringSplit[FileBaseName[file], "."]}];
    records = Import[file, "RawJSON"];
    ParallelMap[ScpDownload[#, save]&, records]
]

End[];