
ScpMain::usage="";

Begin["`Main`"];

ScpMain[]:=Quiet[Flatten[ScpFromIndex /@ $ScpMain] /. $ScpMainModify];



$ScpMain={
    "//scp-wiki-cn.wikidot.com/scp-series",
    "//scp-wiki-cn.wikidot.com/scp-series-2",
    "//scp-wiki-cn.wikidot.com/scp-series-3",
    "//scp-wiki-cn.wikidot.com/scp-series-4"
};
$ScpMainModify={



    "//scp-wiki-cn.wikidot.com/1231-warning"->"//scp-wiki-cn.wikidot.com/scp-1231",
    "//scp-wiki-cn.wikidot.com/scp-2615-j"->"//scp-wiki-cn.wikidot.com/scp-2615"


};
ScpFromIndex[index_]:=Block[
    {
        contents=FirstCase[
            URLExecute[index,{"HTML","XMLObject"}],
            XMLElement["div",{"class"->"content-panel standalone series"},__],0,Infinity
        ]
    },

    Cases[contents,XMLElement["li",{},
        {XMLElement["a",{"shape"->"rect","href"->url_},{num_}],name_}
    ]:><|
        "Url"->StringJoin["//scp-wiki-cn.wikidot.com",url],
        "Number"->num,
        "Name"->StringDelete[name," - "]
    |>,Infinity]
];


End[];