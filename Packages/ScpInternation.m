ScpBranch::usage = "";
Begin["`Branch`"];

Options[ScpBranch] = {Normal -> True};
ScpBranch[OptionsPattern[]] := Block[
    {
        all = Quiet@Join[ScpGetCN[], ScpGetOther[]],
        dQ = Or[
            #["Name"] === "[禁止访问]",
            !StringContainsQ[#["Number"], "-" ~~ __ ~~ "-"],
            !NumberQ[ToExpression@StringTake[#["Number"], -1]]
        ]&;
    },
    If[OptionValue[Normal],
        SortBy[Join[DeleteCases[all, _?dQ], $ScpAdd], #["Number"]&],
        {Select[all, StringTake[#["Number"], -1] === "J"&], Select[all, dQ]}
    ]
];





ScpGetCN[] := Block[
    {
        link = "//scp-wiki-cn.wikidot.com/scp-international",
        site, case1
    },
    link = "//scp-wiki-cn.wikidot.com/scp-series-cn";
    site = URLExecute[link, {"HTML", "XMLObject"}];
    case1 = Cases[site,
        XMLElement["li", {},
            {XMLElement["a", {"shape" -> "rect", "href" -> url_}, {num_}], name_}
        ] :> <|
            "Url" -> StringJoin["//scp-wiki-cn.wikidot.com", url],
            "Number" -> ScpNationNumber[num],
            "Name" -> StringDelete[name, " - "]
        |>,
        Infinity
    ];
    case1
];
ScpNationNumber[input_] := Block[
    {sp = StringSplit[input, "-"]},
    If[And[Length@sp == 3, NumberQ[ToExpression@sp[[2]]]], "SCP-" <> sp[[3]] <> "-" <> sp[[2]], input]
];
ScpGetOther[] := Block[
    {
        link = "//scp-wiki-cn.wikidot.com/scp-international",
        site, case1, case2
    },
    site = URLExecute[link, {"HTML", "XMLObject"}];
    case1 = Cases[site,
        XMLElement["li", {},
            {XMLElement["a", {"shape" -> "rect", "href" -> url_}, {num_}], name_}
        ] :> <|
            "Url" -> StringJoin["//scp-wiki-cn.wikidot.com", url],
            "Number" -> ScpNationNumber[num],
            "Name" -> StringDelete[name, " - "]
        |>,
        Infinity
    ];
    case2 = SequenceCases[
        Cases[site,
            XMLElement["p", {}, {x__}] :> x, Infinity], {
            XMLElement["a", {__, "href" -> url_}, {num_}], name_, XMLElement["br", ___]
        } :> <|
            "Url" -> StringJoin["//scp-wiki-cn.wikidot.com", url],
            "Number" -> ScpNationNumber[num],
            "Name" -> StringDelete[name, " - "]
        |>];
    Join[case1, case2]
];
$ScpAdd = {
    <|"Url" -> "//scp-wiki-cn.wikidot.com/scp-cn-066", "Number" -> "SCP-CN-066", "Name" -> "居然对八旬老人做出这种不知羞耻的..."|>,
    <|"Url" -> "//scp-wiki-cn.wikidot.com/scp-cn-296", "Number" -> "SCP-CN-296", "Name" -> "艾斯 瑟 癖 横杠 瑟 恩 横杠 二九六"|>
};



End[]