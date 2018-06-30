

ScpGetContent::usage="";



Begin["`Core`"];




ScpGetContent[url_]:=Block[
    {
        get=URLExecute[url,{"HTML","XMLObject"}],
        rank0,rank1,rank2,rank3,md
    },
    rank0=FirstCase[get,XMLElement["div",{"style"->"min-height:600px\""},a__]:>a,0,Infinity];
    rank1=rank0/.{
        XMLElement["br", ___]:>Nothing,
        XMLElement["div",___]:>Nothing,
        XMLElement["img",___]:>Nothing,
        XMLElement["sub",___]:>Nothing,
        XMLElement["hr", ___]:>"\n---\n",
        XMLElement["em",{},{s_}]:>Sequence["*",s,"* "],
        XMLElement["strong",{},{s_}]:>Sequence["**",s,"** "],
        XMLElement["li", {}, {li_}]:>Sequence["- ",li,"\n"]
    (*XMLElement["span", _, {s_}]\[RuleDelayed]s*)
    };
    rank2=rank1/.{
        XMLElement["h1",_, {h_}]:>Sequence["# ",h,"\n"],
        XMLElement["h2",_, {h_}]:>Sequence["## ",h,"\n"],
        XMLElement["h3",_, {h_}]:>Sequence["### ",h,"\n"],
        XMLElement["h4",_, {h_}]:>Sequence["#### ",h,"\n"],
        XMLElement["h5",_, {h_}]:>Sequence["##### ",h,"\n"],
        XMLElement["h6",_, {h_}]:>Sequence["###### ",h,"\n"],
        XMLElement["a", {"shape"->"rect", "href" ->h_}, {m_}]:>Sequence["[",m,"](",StringJoin["//scp-wiki-cn.wikidot.com",h],")"],
        XMLElement["ul", {},ul_]:>ul,
        XMLElement["blockquote",{},q_]:>Sequence@@Riffle[q, "> "]
    };
    md=rank2/.{
        XMLElement["p",{},{p__}]:>Sequence[ p,"\n"]
    };
    Flatten[md/.XMLElement[xml__]:>ExportString[XMLElement@xml,"XML"]]
];





End[];