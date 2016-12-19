module Rules (canHyphenate, exempt) where

import           Data.List            (intersect)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict as M (lookup, fromAscList)
import           Data.Maybe           (fromMaybe)

-- Empty string is not exempt ?! !?
exempt :: String -> Bool
exempt s = not . null $ s`intersect`noHyp
  where noHyp = "/\\0123456789()[]{}!@#$%^&*=_><'`"

canHyphenate :: Char -> Char -> Bool
canHyphenate c1 c2 = fromMaybe False $ M.lookup (c1:c2:[]) rules

-- fromAscList: O(n). Build a map from  an ascending list in lin-
-- ear time. The precondition  (input  list  is ascending) is not
-- checked.
rules :: Map String Bool
rules = M.fromAscList
    [("aa",False),("ab",True ),("ac",True ),("ad",True ),("ae",False),("af",True ),
     ("ag",True ),("ah",False),("ai",False),("aj",True ),("ak",True ),("al",True ),
     ("am",True ),("an",True ),("ao",False),("ap",True ),("aq",True ),("ar",True ),
     ("as",True ),("at",True ),("au",False),("av",True ),("aw",True ),("ax",True ),
     ("ay",False),("az",True ),("ba",True ),("bb",False),("bc",False),("bd",True ),
     ("be",True ),("bf",True ),("bg",True ),("bh",True ),("bi",True ),("bj",True ),
     ("bk",True ),("bl",True ),("bm",True ),("bn",True ),("bo",True ),("bp",True ),
     ("bq",True ),("br",True ),("bs",True ),("bt",True ),("bu",True ),("bv",True ),
     ("bw",True ),("bx",True ),("by",True ),("bz",True ),("ca",True ),("cb",True ),
     ("cc",False),("cd",True ),("ce",False),("cf",True ),("cg",True ),("ch",False),
     ("ci",False),("cj",True ),("ck",False),("cl",True ),("cm",True ),("cn",True ),
     ("co",True ),("cp",True ),("cq",True ),("cr",True ),("cs",True ),("ct",True ),
     ("cu",True ),("cv",True ),("cw",True ),("cx",True ),("cy",False),("cz",True ),
     ("da",True ),("db",True ),("dc",True ),("dd",True ),("de",True ),("df",True ),
     ("dg",True ),("dh",True ),("di",True ),("dj",True ),("dk",True ),("dl",True ),
     ("dm",True ),("dn",True ),("do",True ),("dp",True ),("dq",True ),("dr",True ),
     ("ds",True ),("dt",True ),("du",True ),("dv",True ),("dw",True ),("dx",True ),
     ("dy",True ),("dz",True ),("ea",False),("eb",True ),("ec",True ),("ed",True ),
     ("ee",False),("ef",True ),("eg",True ),("eh",True ),("ei",False),("ej",True ),
     ("ek",True ),("el",True ),("em",True ),("en",True ),("eo",True ),("ep",True ),
     ("eq",True ),("er",True ),("es",True ),("et",True ),("eu",False),("ev",True ),
     ("ew",True ),("ex",False),("ey",False),("ez",True ),("fa",True ),("fb",True ),
     ("fc",True ),("fd",True ),("fe",True ),("ff",False),("fg",True ),("fh",True ),
     ("fi",True ),("fj",True ),("fk",True ),("fl",True ),("fm",True ),("fn",True ),
     ("fo",True ),("fp",True ),("fq",True ),("fr",True ),("fs",True ),("ft",True ),
     ("fu",True ),("fv",True ),("fw",True ),("fx",True ),("fy",True ),("fz",True ),
     ("ga",True ),("gb",True ),("gc",True ),("gd",True ),("ge",True ),("gf",True ),
     ("gg",False),("gh",True ),("gi",True ),("gj",True ),("gk",True ),("gl",True ),
     ("gm",True ),("gn",True ),("go",True ),("gp",True ),("gq",True ),("gr",True ),
     ("gs",True ),("gt",True ),("gu",True ),("gv",True ),("gw",True ),("gx",True ),
     ("gy",True ),("gz",True ),("ha",True ),("hb",True ),("hc",True ),("hd",True ),
     ("he",True ),("hf",True ),("hg",True ),("hh",True ),("hi",True ),("hj",True ),
     ("hk",True ),("hl",True ),("hm",True ),("hn",True ),("ho",True ),("hp",True ),
     ("hq",True ),("hr",True ),("hs",True ),("ht",True ),("hu",True ),("hv",True ),
     ("hw",True ),("hx",True ),("hy",True ),("hz",True ),("ia",True ),("ib",True ),
     ("ic",True ),("id",True ),("ie",False),("if",True ),("ig",True ),("ih",True ),
     ("ii",True ),("ij",True ),("ik",True ),("il",True ),("im",True ),("in",True ),
     ("io",True ),("ip",True ),("iq",True ),("ir",True ),("is",True ),("it",True ),
     ("iu",True ),("iv",True ),("iw",True ),("ix",True ),("iy",True ),("iz",True ),
     ("ja",True ),("jb",True ),("jc",True ),("jd",True ),("je",True ),("jf",True ),
     ("jg",True ),("jh",True ),("ji",True ),("jj",False),("jk",True ),("jl",True ),
     ("jm",True ),("jn",True ),("jo",True ),("jp",True ),("jq",True ),("jr",True ),
     ("js",True ),("jt",True ),("ju",True ),("jv",True ),("jw",True ),("jx",True ),
     ("jy",True ),("jz",True ),("ka",True ),("kb",True ),("kc",True ),("kd",True ),
     ("ke",True ),("kf",True ),("kg",True ),("kh",True ),("ki",True ),("kj",True ),
     ("kk",False),("kl",True ),("km",True ),("kn",True ),("ko",True ),("kp",True ),
     ("kq",True ),("kr",True ),("ks",True ),("kt",True ),("ku",True ),("kv",True ),
     ("kw",True ),("kx",True ),("ky",True ),("kz",True ),("la",True ),("lb",True ),
     ("lc",True ),("ld",True ),("le",True ),("lf",True ),("lg",True ),("lh",True ),
     ("li",True ),("lj",True ),("lk",True ),("ll",False),("lm",True ),("ln",True ),
     ("lo",True ),("lp",True ),("lq",True ),("lr",True ),("ls",True ),("lt",True ),
     ("lu",True ),("lv",True ),("lw",True ),("lx",True ),("ly",True ),("lz",True ),
     ("ma",True ),("mb",True ),("mc",True ),("md",True ),("me",True ),("mf",True ),
     ("mg",True ),("mh",True ),("mi",True ),("mj",True ),("mk",True ),("ml",True ),
     ("mm",False),("mn",True ),("mo",True ),("mp",True ),("mq",True ),("mr",True ),
     ("ms",True ),("mt",True ),("mu",True ),("mv",True ),("mw",True ),("mx",True ),
     ("my",True ),("mz",True ),("na",True ),("nb",True ),("nc",True ),("nd",True ),
     ("ne",True ),("nf",True ),("ng",False),("nh",True ),("ni",True ),("nj",True ),
     ("nk",True ),("nl",True ),("nm",True ),("nn",False),("no",True ),("np",True ),
     ("nq",True ),("nr",True ),("ns",True ),("nt",False),("nu",True ),("nv",True ),
     ("nw",True ),("nx",True ),("ny",True ),("nz",True ),("oa",True ),("ob",True ),
     ("oc",True ),("od",True ),("oe",True ),("of",True ),("og",True ),("oh",True ),
     ("oi",False),("oj",True ),("ok",True ),("ol",True ),("om",True ),("on",True ),
     ("oo",False),("op",True ),("oq",True ),("or",True ),("os",True ),("ot",True ),
     ("ou",False),("ov",True ),("ow",True ),("ox",True ),("oy",True ),("oz",True ),
     ("pa",True ),("pb",True ),("pc",True ),("pd",True ),("pe",True ),("pf",True ),
     ("pg",True ),("ph",False),("pi",True ),("pj",True ),("pk",True ),("pl",True ),
     ("pm",True ),("pn",True ),("po",True ),("pp",False),("pq",True ),("pr",True ),
     ("ps",True ),("pt",False),("pu",True ),("pv",True ),("pw",True ),("px",True ),
     ("py",True ),("pz",True ),("qa",True ),("qb",True ),("qc",True ),("qd",True ),
     ("qe",True ),("qf",True ),("qg",True ),("qh",True ),("qi",True ),("qj",True ),
     ("qk",True ),("ql",True ),("qm",True ),("qn",True ),("qo",True ),("qp",True ),
     ("qq",False),("qr",True ),("qs",True ),("qt",True ),("qu",False),("qv",True ),
     ("qw",True ),("qx",True ),("qy",True ),("qz",True ),("ra",True ),("rb",True ),
     ("rc",True ),("rd",True ),("re",True ),("rf",True ),("rg",True ),("rh",False),
     ("ri",True ),("rj",True ),("rk",True ),("rl",True ),("rm",True ),("rn",True ),
     ("ro",True ),("rp",True ),("rq",True ),("rr",False),("rs",True ),("rt",True ),
     ("ru",True ),("rv",True ),("rw",True ),("rx",True ),("ry",True ),("rz",True ),
     ("sa",True ),("sb",True ),("sc",False),("sd",True ),("se",True ),("sf",True ),
     ("sg",True ),("sh",False),("si",True ),("sj",True ),("sk",True ),("sl",True ),
     ("sm",True ),("sn",True ),("so",True ),("sp",True ),("sq",True ),("sr",True ),
     ("ss",False),("st",True ),("su",True ),("sv",True ),("sw",True ),("sx",True ),
     ("sy",True ),("sz",True ),("ta",True ),("tb",True ),("tc",True ),("td",True ),
     ("te",True ),("tf",True ),("tg",True ),("th",False),("ti",True ),("tj",True ),
     ("tk",True ),("tl",True ),("tm",True ),("tn",True ),("to",True ),("tp",True ),
     ("tq",True ),("tr",True ),("ts",True ),("tt",False),("tu",True ),("tv",True ),
     ("tw",True ),("tx",True ),("ty",True ),("tz",True ),("ua",True ),("ub",True ),
     ("uc",True ),("ud",True ),("ue",False),("uf",True ),("ug",True ),("uh",False),
     ("ui",False),("uj",True ),("uk",True ),("ul",True ),("um",True ),("un",True ),
     ("uo",True ),("up",True ),("uq",True ),("ur",True ),("us",True ),("ut",True ),
     ("uu",False),("uv",True ),("uw",True ),("ux",True ),("uy",True ),("uz",True ),
     ("va",True ),("vb",True ),("vc",True ),("vd",True ),("ve",True ),("vf",True ),
     ("vg",True ),("vh",True ),("vi",True ),("vj",True ),("vk",True ),("vl",True ),
     ("vm",True ),("vn",True ),("vo",True ),("vp",True ),("vq",True ),("vr",True ),
     ("vs",True ),("vt",True ),("vu",True ),("vv",False),("vw",True ),("vx",True ),
     ("vy",False),("vz",True ),("wa",True ),("wb",True ),("wc",True ),("wd",True ),
     ("we",True ),("wf",True ),("wg",True ),("wh",False),("wi",True ),("wj",True ),
     ("wk",True ),("wl",True ),("wm",True ),("wn",True ),("wo",True ),("wp",True ),
     ("wq",True ),("wr",False),("ws",True ),("wt",True ),("wu",True ),("wv",True ),
     ("ww",False),("wx",True ),("wy",True ),("wz",True ),("xa",True ),("xb",True ),
     ("xc",True ),("xd",True ),("xe",True ),("xf",True ),("xg",True ),("xh",True ),
     ("xi",True ),("xj",True ),("xk",True ),("xl",True ),("xm",True ),("xn",True ),
     ("xo",True ),("xp",True ),("xq",True ),("xr",True ),("xs",True ),("xt",False),
     ("xu",True ),("xv",True ),("xw",True ),("xx",True ),("xy",True ),("xz",True ),
     ("ya",True ),("yb",True ),("yc",True ),("yd",True ),("ye",True ),("yf",True ),
     ("yg",True ),("yh",True ),("yi",True ),("yj",True ),("yk",True ),("yl",True ),
     ("ym",True ),("yn",True ),("yo",True ),("yp",True ),("yq",True ),("yr",True ),
     ("ys",True ),("yt",True ),("yu",True ),("yv",True ),("yw",True ),("yx",True ),
     ("yy",False),("yz",True ),("za",True ),("zb",True ),("zc",True ),("zd",True ),
     ("ze",True ),("zf",True ),("zg",True ),("zh",False),("zi",True ),("zj",True ),
     ("zk",True ),("zl",True ),("zm",True ),("zn",True ),("zo",True ),("zp",True ),
     ("zq",True ),("zr",True ),("zs",True ),("zt",True ),("zu",True ),("zv",True ),
     ("zw",True ),("zx",True ),("zy",True ),("zz",False)]
