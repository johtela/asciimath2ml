(()=>{var q=Object.create;var T=Object.defineProperty;var N=Object.getOwnPropertyDescriptor;var z=Object.getOwnPropertyNames;var U=Object.getPrototypeOf,j=Object.prototype.hasOwnProperty;var I=(r,t)=>()=>(t||r((t={exports:{}}).exports,t),t.exports);var W=(r,t,u,i)=>{if(t&&typeof t=="object"||typeof t=="function")for(let D of z(t))!j.call(r,D)&&D!==u&&T(r,D,{get:()=>t[D],enumerable:!(i=N(t,D))||i.enumerable});return r};var Z=(r,t,u)=>(u=r!=null?q(U(r)):{},W(t||!r||!r.__esModule?T(u,"default",{value:r,enumerable:!0}):u,r));var M=I(c=>{"use strict";Object.defineProperty(c,"__esModule",{value:!0});c.StyledElement=c.CustomElement=void 0;var S=class extends HTMLElement{constructor(){super(),this.root=this.attachShadow({mode:"open"}),this.connected=!1}connectedCallback(){this.connected||(this.connect(),this.connected=!0)}};c.CustomElement=S;var k=class extends S{constructor(t){super();let u=document.createElement("link");u.setAttribute("rel","stylesheet");let i=document.currentScript.src,D=i.substring(0,i.lastIndexOf("/"));u.setAttribute("href",`${D}/${t}.css`),this.root.appendChild(u),this.body=document.createElement("div"),this.root.appendChild(this.body)}};c.StyledElement=k});var H=Z(M());var B=class{constructor(t,u,i){this.charTables=[];this.input=t,this.symbols=u,this.escapePunctuation=i,this.pos=0}eof(){return this.pos>=this.input.length}skipWhitespace(){for(;this.pos<this.input.length&&/\s/.test(this.input[this.pos]);)++this.pos;return this.pos<this.input.length?this.pos:-1}peekSymbol(){let t=this.skipWhitespace();if(t<0)return[Y(),t];let u=this.input[t];if(u=='"'){for(;++t<this.input.length&&this.input[t]!='"';);let D=this.input.slice(this.pos+1,t);return this.escapePunctuation&&(D=D.replace(/[^A-Za-z0-9]/g,o=>`&#${o.charCodeAt(0)};`)),[J(D),t+1]}if(/\d/.test(u)){for(;t<this.input.length&&/[\d\.]/.test(this.input[t]);)++t;return[V(this.input.slice(this.pos,t)),t]}let i=this.symbols[u];if(i)for(let D=0;D<i.length;++D){let o=i[D],a=o.input.length;if(this.input.slice(t,t+a)==o.input)return[o,t+a]}return[F(u),t+1]}nextSymbol(){let[t,u]=this.peekSymbol();return u>=0&&(this.pos=u),t}pushCharTable(t){this.charTables.push(t)}popCharTable(){this.charTables.pop()}charTable(){return this.charTables[this.charTables.length-1]}},Q=["\u{1D49C}","\u212C","\u{1D49E}","\u{1D49F}","\u2130","\u2131","\u{1D4A2}","\u210B","\u2110","\u{1D4A5}","\u{1D4A6}","\u2112","\u2133","\u{1D4A9}","\u{1D4AA}","\u{1D4AB}","\u{1D4AC}","\u211B","\u{1D4AE}","\u{1D4AF}","\u{1D4B0}","\u{1D4B1}","\u{1D4B2}","\u{1D4B3}","\u{1D4B4}","\u{1D4B5}","\u{1D4B6}","\u{1D4B7}","\u{1D4B8}","\u{1D4B9}","\u212F","\u{1D4BB}","\u210A","\u{1D4BD}","\u{1D4BE}","\u{1D4BF}","\u{1D4C0}","\u{1D4C1}","\u{1D4C2}","\u{1D4C3}","\u2134","\u{1D4C5}","\u{1D4C6}","\u{1D4C7}","\u{1D4C8}","\u{1D4C9}","\u{1D4CA}","\u{1D4CB}","\u{1D4CC}","\u{1D4CD}","\u{1D4CE}","\u{1D4CF}"],G=["\u{1D504}","\u{1D505}","\u212D","\u{1D507}","\u{1D508}","\u{1D509}","\u{1D50A}","\u210C","\u2111","\u{1D50D}","\u{1D50E}","\u{1D50F}","\u{1D510}","\u{1D511}","\u{1D512}","\u{1D513}","\u{1D514}","\u211C","\u{1D516}","\u{1D517}","\u{1D518}","\u{1D519}","\u{1D51A}","\u{1D51B}","\u{1D51C}","\u2128","\u{1D51E}","\u{1D51F}","\u{1D520}","\u{1D521}","\u{1D522}","\u{1D523}","\u{1D524}","\u{1D525}","\u{1D526}","\u{1D527}","\u{1D528}","\u{1D529}","\u{1D52A}","\u{1D52B}","\u{1D52C}","\u{1D52D}","\u{1D52E}","\u{1D52F}","\u{1D530}","\u{1D531}","\u{1D532}","\u{1D533}","\u{1D534}","\u{1D535}","\u{1D536}","\u{1D537}"],X=["\u{1D538}","\u{1D539}","\u2102","\u{1D53B}","\u{1D53C}","\u{1D53D}","\u{1D53E}","\u210D","\u{1D540}","\u{1D541}","\u{1D542}","\u{1D543}","\u{1D544}","\u2115","\u{1D546}","\u2119","\u211A","\u211D","\u{1D54A}","\u{1D54B}","\u{1D54C}","\u{1D54D}","\u{1D54E}","\u{1D54F}","\u{1D550}","\u2124","\u{1D552}","\u{1D553}","\u{1D554}","\u{1D555}","\u{1D556}","\u{1D557}","\u{1D558}","\u{1D559}","\u{1D55A}","\u{1D55B}","\u{1D55C}","\u{1D55D}","\u{1D55E}","\u{1D55F}","\u{1D560}","\u{1D561}","\u{1D562}","\u{1D563}","\u{1D564}","\u{1D565}","\u{1D566}","\u{1D567}","\u{1D568}","\u{1D569}","\u{1D56A}","\u{1D56B}"];function K(r,t){if(!t)return r;let u="";for(let i=0;i<r.length;++i){let D=r.charCodeAt(i);u+=D>=65&&D<91?t[D-65]:D>=97&&D<123?t[D-71]:r[i]}return u}function J(r){return{kind:0,input:r,parser:t=>`<mtext>${K(r,t.charTable())}</mtext>`}}function V(r){return{kind:0,input:r,parser:()=>`<mn>${r}</mn>`}}function F(r){return{kind:0,input:"",parser:()=>`<merror><mtext>${r}</mtext></merror>`}}function Y(){return{kind:8,input:"",parser:()=>""}}function n(r,t=r){return{kind:0,input:r,parser:u=>`<mi>${K(t,u.charTable())}</mi>`}}function e(r,t){return{kind:0,input:r,parser:()=>`<mo>${t}</mo>`}}function p(r,t=r){return{kind:0,input:r,parser:()=>`<mrow><mspace width="1ex"/><mtext>${t}</mtext><mspace width="1ex"/></mrow>`}}function l(r,t=r){return{kind:1,input:r,parser:()=>`<mo>${t}</mo>`}}function d(r,t){return{kind:2,input:r,parser:t?()=>`<mo>${t}</mo>`:()=>""}}function h(r,t){return{kind:3,input:r,parser:t?()=>`<mo>${t}</mo>`:()=>""}}function tt(r){return t=>{let u=m(t);return`<mrow>${r}${u}</mrow>`}}function s(r,t=r){return{kind:0,input:r,parser:tt(`<mo>${t}</mo>`)}}function rt(r){return t=>{let u=m(t);return`<${r}>${u}</${r}>`}}function P(r,t){return{kind:0,input:r,parser:rt(t)}}function et(r,t){return u=>{let i=m(u);return`<${r}>${i}${t}</${r}>`}}function x(r,t,u){return{kind:1,input:r,parser:et(t,`<mo>${u}</mo>`)}}function ut(r,t){return u=>{let i=m(u);return`<mrow>${r}${i}${t}</mrow>`}}function f(r,t,u){return{kind:0,input:r,parser:ut(`<mo>${t}</mo>`,`<mo>${u}</mo>`)}}function nt(r,t){return u=>{let i=m(u);return`<${r} ${t}">${i}</${r}>`}}function A(r,t,u){return{kind:0,input:r,parser:nt(t,u)}}function it(r){return t=>{t.pushCharTable(r);let u=m(t);return t.popCharTable(),u}}function v(r,t){return{kind:0,input:r,parser:it(t)}}function Dt(r){return t=>{let u=m(t),i=m(t);return`<${r}>${u}${i}</${r}>`}}function y(r,t){return{kind:0,input:r,parser:Dt(t)}}function st(r,t){return u=>{let i=u.nextSymbol().input,D=m(u);return`<${r} ${t}="${i}">${D}</${r}>`}}function $(r,t,u){return{kind:0,input:r,parser:st(t,u)}}function O(r){let t=r.nextSymbol();if(t.kind==2){let u=t.parser(r),[i]=r.peekSymbol(),D=i.kind==3?"":w(r);i=r.nextSymbol();let o=(i.kind==3?i:F("Missing closing paren")).parser(r);return[`<mrow>${u}${D}${o}</mrow>`,t]}return[t.parser(r),t]}function m(r){return O(r)[0]}function L(r){let[t,u]=O(r),i,D,[o,a]=r.peekSymbol();return o.input=="_"&&(r.pos=a,i=m(r),[o,a]=r.peekSymbol()),o.input=="^"&&(r.pos=a,D=m(r)),u.kind==1?i&&D?`<munderover>${t}${i}${D}</munderover>`:i?`<munder>${t}${i}</munder>`:D?`<mover>${t}${D}</mover>`:t:i&&D?`<msubsup>${t}${i}${D}</msubsup>`:i?`<msub>${t}${i}</msub>`:D?`<msup>${t}${D}</msup>`:t}var R=[8,3,6,7,5];function w(r){let t="";for(;;){let u=L(r),[i,D]=r.peekSymbol();if(R.includes(i.kind))return t+u;if(i.input=="/"){r.pos=D;let o=L(r);if(u=`<mfrac>${u}${o}</mfrac>`,[i]=r.peekSymbol(),R.includes(i.kind))return t+u}t+=u}}function ot(r){return t=>{let u="";for(;;){let[i,D]=t.peekSymbol();if(i.kind==8||i.kind==5){t.pos=D;let a=i.parser(t);return r||a?`<mrow>${r}<mtable>${u}</mtable>${a}</mrow>`:`<mtable>${u}</mtable>`}let o=at(t);u=`${u}<mtr>${o}</mtr>`}}}function at(r){let t="";for(;;){let[u,i]=r.peekSymbol();if(u.kind==8||u.kind==7)return r.pos=i,t;if(u.kind==5)return t;let D=w(r);t=`${t}<mtd>${D}</mtd>`}}function g(r,t){return{kind:4,input:r,parser:ot(t?`<mo>${t}</mo>`:"")}}function C(r,t){return{kind:5,input:r,parser:()=>t?`<mo>${t}</mo>`:""}}function lt(r){return{kind:6,input:r,parser:()=>""}}function mt(r){return{kind:7,input:r,parser:()=>""}}var xt={a:[s("arcsin"),s("arccos"),s("arctan"),n("alpha","&#x03B1;"),e("aleph","&#x2135;"),f("abs","&#124;","&#124;"),p("and"),n("a")],A:[s("Arcsin"),s("Arccos"),s("Arctan"),f("Abs","&#124;","&#124;"),e("AA","&#x2200;"),n("A")],b:[n("beta","&#x03B2;"),x("bar","mover","&#x00AF;"),v("bbb",X),A("bb","mstyle",'style="font-weight: bold"'),n("b")],B:[n("B")],c:[A("cancel","menclose",'notation="updiagonalstrike"'),$("color","mstyle","mathcolor"),$("class","mrow","class"),e("cdots","&#x22EF;"),f("ceil","&#x2308;","&#x2309;"),s("cosh"),s("csch"),s("cos"),s("cot"),s("csc"),n("chi","&#x03C7;"),v("cc",Q),n("c")],C:[s("Cosh"),s("Cos"),s("Cot"),s("Csc"),e("CC","&#x2102;"),n("C")],d:[e("diamonds","&#x22C4;"),n("delta","&#x03B4;"),e("ddots","&#x22F1;"),x("ddot","mover",".."),e("darr","&#x2193;"),e("del","&#x2202;"),s("det"),x("dot","mover","."),p("dim"),n("d")],D:[e("Delta","&#x0394;"),n("D")],e:[n("epsilon","&#x03B5;"),n("eta","&#x03B7;"),s("exp"),n("e")],E:[e("EE","&#x2203;"),n("E")],f:[f("floor","&#x230A;","&#x230B;"),e("frown","&#x2322;"),y("frac","mfrac"),v("fr",G),n("f")],F:[n("F")],g:[n("gamma","&#x03B3;"),e("grad","&#x2207;"),s("gcd"),p("glb"),n("g")],G:[e("Gamma","&#x0393;"),n("G")],h:[e("harr","&#x2194;"),e("hArr","&#x21D4;"),x("hat","mover","&#x005E;"),n("h")],H:[n("H")],i:[n("iota","&#x03B9;"),e("int","&#x222B;"),e("in","&#x2208;"),p("if"),$("id","mrow","id"),n("i")],I:[n("I")],j:[n("j")],J:[n("J")],k:[n("kappa","&#x03BA;"),n("k")],K:[n("K")],l:[n("lambda","&#x03BB;"),e("larr","&#x2190;"),e("lArr","&#x21D0;"),l("lim","lim"),s("log"),s("lcm"),p("lub"),s("ln"),n("l")],L:[e("Lambda","&#x039B;"),l("Lim","Lim"),s("Log"),s("Ln"),n("L")],m:[l("min"),l("max"),p("mod"),n("mu","&#x03BC;"),n("m")],M:[n("M")],n:[f("norm","&#x2225;","&#x2225;"),l("nnn","&#x22C2;"),e("not","&#x00AC;"),e("nn","&#x2229;"),n("nu","&#x03BD;"),n("n")],N:[e("NN","&#x2115;"),n("N")],o:[x("overarc","mover","&#x23DC;"),y("overset","mover"),x("obrace","mover","&#x23DE;"),n("omega","&#x03C9;"),e("oint","&#x222E;"),p("or"),e("o+","&#x2295;"),e("ox","&#x2295;"),e("o.","&#x2299;"),e("oo","&#x221E;"),n("o")],O:[e("Omega","&#x03A9;"),e("O/","&#x2205;"),n("O")],p:[l("prod","&#x220F;"),n("prop","&#x221D;"),n("phi","&#x03D5;"),n("psi","&#x03C8;"),n("pi","&#x03C0;"),n("p")],P:[e("Phi","&#x03A6;"),n("Psi","&#x03A8;"),e("Pi","&#x03A0;"),n("P")],q:[e("qquad","\xA0\xA0\xA0\xA0"),e("quad","\xA0\xA0"),n("q")],Q:[e("QQ","&#x211A;"),n("Q")],r:[e("rarr","&#x2192;"),e("rArr","&#x21D2;"),y("root","mroot"),n("rho","&#x03C1;"),n("r")],R:[e("RR","&#x211D;"),n("R")],s:[y("stackrel","mover"),e("setminus","&#92;"),e("square","&#x25A1;"),n("sigma","&#x03C3;"),l("sube","&#x2286;"),l("supe","&#x2287;"),P("sqrt","msqrt"),s("sinh"),s("sech"),l("sum","&#x2211;"),l("sub","&#x2282;"),l("sup","&#x2283;"),s("sin"),s("sec"),A("sf","mstyle",'style="font-family: sans-serif"'),n("s")],S:[e("Sigma","&#x03A3;"),s("Sinh"),s("Sin"),s("Sec"),n("S")],t:[n("theta","&#x03B8;"),x("tilde","mover","&#126;"),P("text","mtext"),s("tanh"),s("tan"),n("tau","&#x03C4;"),A("tt","mstyle",'style="font-family: monospace"'),n("t")],T:[e("Theta","&#x0398;"),s("Tanh"),s("Tan"),e("TT","&#x22A4;"),n("T")],u:[y("underset","munder"),n("upsilon","&#x03C5;"),x("ubrace","munder","&#x23DF;"),e("uarr","&#x2191;"),l("uuu","&#x22C3;"),e("uu","&#x222A;"),x("ul","munder","&#x0332;"),n("u")],U:[n("U")],v:[n("varepsilon","&#x025B;"),n("vartheta","&#x03D1;"),n("varphi","&#x03C6;"),e("vdots","&#x22EE;"),x("vec","mover","&#x2192;"),l("vvv","&#x22C1;"),e("vv","&#x2228;"),n("v")],V:[n("V")],w:[n("w")],W:[n("W")],x:[n("xi","&#x03BE;"),e("xx","&#x00D7;"),n("x")],X:[n("Xi","&#x039E;"),n("X")],y:[n("y")],Y:[n("Y")],z:[n("zeta","&#x03B6;"),n("z")],Z:[e("ZZ","&#x2124;"),n("Z")],"-":[e("__|","&#x230B;"),e("-<=","&#x2AAF;"),e("->>","&#x21A0;"),e("->","&#x2192;"),e("-<","&#x227A;"),e("-:","&#x00F7;"),e("-=","&#x2261;"),e("-+","&#x2213;"),e("-","&#x2212;")],"*":[e("***","&#x22C6;"),e("**","&#x2217;"),e("*","&#x22C5;")],"+":[e("+-","&#x00B1;"),e("+","&#43;")],"/":[e("/_\\","&#x25B3;"),e("/_","&#x2220;"),e("//","&#47;"),e("/","")],"\\":[e("\\\\","&#92;"),e("\\","&#x00A0;")],"|":[e("|><|","&#x22C8;"),e("|><","&#x22C9;"),e("|->","&#x21A6;"),e("|--","&#x22A2;"),e("|==","&#x22A8;"),e("|__","&#x230A;"),g("||:","&#124;"),g("|::"),e("|~","&#x2308;"),d("|:","&#124;"),C("|)","&#41;"),C("|]","&#93;"),C("|}","&#125;"),e("|","&#124;")],"<":[e("<=>","&#x21D4;"),e("<=","&#x2264;"),e("<<","&#x226A;"),e("<","&#60;")],">":[e(">->>","&#x2916;"),e(">->","&#x21A3;"),e("><|","&#x22CA;"),e(">-=","&#x2AB0;"),e(">=","&#x2265;"),e(">-","&#x227B;"),e(">>","&#x226B;"),e(">","&#62;")],"=":[e("=>","&#x21D2;"),e("=","&#61;")],"@":[e("@","&#x2218;")],"^":[l("^^^","&#x22C0;"),e("^^","&#x2227;"),e("^","")],"~":[e("~~","&#x2248;"),e("~=","&#x2245;"),e("~|","&#x2309;"),e("~","&#x223C;")],"!":[e("!in","&#x2209;"),e("!=","&#x2260;"),e("!","&#33;")],":":[C(":||","&#124;"),C("::|"),e(":=","&#58;&#61;"),h(":)","&#x232A;"),h(":|","&#124;"),h(":}","&#125;"),e(":.","&#x2234;"),e(":'","&#x2235;"),e(":","&#58;")],";":[mt(";;"),lt(";")],".":[e("...","&#46;&#46;&#46;")],",":[e(",","&#44;")],_:[e("_|_","&#x22A5;"),e("_","")],"'":[e("'","&#x2032;")],"(":[g("(|","&#40;"),d("(:","&#x2329;"),d("(","&#40;")],")":[h(")","&#41;")],"[":[g("[|","&#91;"),d("[","&#91;")],"]":[h("]","&#93;")],"{":[g("{|","&#123;"),d("{:","&#123;"),d("{")],"}":[h("}")]};function _(r,t=!1,u=!1){let i=new B(r,xt,u);return`<math display="${t?"inline":"block"}"><mstyle displaystyle="true">${w(i)}</mstyle></math>`}var E=class extends H.StyledElement{constructor(){super("asciimath-editor");this.label=`
            <span class="label">Preview</span>
    `;this.content=`
        <textarea name="input" id="input" rows="5" spellcheck="false"
            placeholder="Enter AsciiMath Equation">
        </textarea>
        <div id="preview">
            ${this.label}
        </div>
    `}get input(){return this.shadowRoot.getElementById("input")}get preview(){return this.shadowRoot.getElementById("preview")}update(){let u=this.input.value,i=this.preview;u?(u=_(u),i.innerHTML=u,this.format(i,0),u=`
                ${u}
                <details>
                    <summary>Show MathML</summary>
                    <xmp>${i.innerHTML.trim()}</xmp>
                </details>
            `):u=this.label,i.innerHTML=u}connect(){this.body.innerHTML=this.content,this.body.className="body",this.body.append(this.input,this.preview);let u=this.getAttribute("value");u&&(this.input.value=u,this.update()),this.input.onchange=()=>this.update()}format(u,i){let D=new Array(i+++1).join("  "),o=new Array(i-1).join("  "),a;for(var b=0;b<u.children.length;b++)a=document.createTextNode(`
`+D),u.insertBefore(a,u.children[b]),this.format(u.children[b],i),u.lastElementChild==u.children[b]&&(a=document.createTextNode(`
`+o),u.appendChild(a));return u}};customElements.define("asciimath-editor",E);})();
