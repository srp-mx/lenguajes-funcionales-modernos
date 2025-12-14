{-# LANGUAGE QuasiQuotes #-}

module CodeGenerator (
    codeGen,
    codeGenInfo
) where

import Expr
import qualified Data.HashSet as HS
import Text.RawString.QQ

-- |Información de la generación de código
codeGenInfo :: String
codeGenInfo = "Generador de código SKI lazy"

-- |Genera el programa objetivo en C++
codeGen :: Expr -> String
codeGen expr =
    let oexpr = optimizeExpr expr
    in prelude
       ++ pregen oexpr
       ++ "inline static Ref<Closure> mkProgram() { return " ++ gen oexpr ++ "; }"
       ++ [r|
int main(int argc, char* argv[]) {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    initInfos();
    initGlobals();
    auto program = mkProgram();
    for (int i = 1; i < argc; ++i) {
        program = app(program, ParseArg(argv[i]));
    }
#ifdef DEBUG
    cerr << Show(program) << "\n";
#endif
    auto r = Evaluate(program);
#ifdef DEBUG
    cerr << Show(r) << "\n";
#else
    if (r->info->tag == Tag::CON) {
        cout << r->s << "\n";
    }
#endif
    return 0;
}|]

-- |Preludio del código a generar
prelude :: String
prelude = [r|// CÓDIGO GENERADO CON srp-mx/lenguajes-funcionales-modernos
#include <bits/stdc++.h>
using namespace std;
struct Closure;
struct Info;
struct Thread;
template<typename T> using Ref = shared_ptr<T>;
enum class Tag { FUN, THUNK, PAP, AP, CON, IND };
struct Info {
    Tag tag;
    int arity; // FUN
    function<void(Thread&, Ref<Closure>)> entry; // FUN/THUNK/AP/PAP
    const char* name;
};
struct Closure {
    Ref<Info> info;
    vector<Ref<Closure>> payload;
    string s; // CON
    Closure(Ref<Info> info_, vector<Ref<Closure>> pay = {}, string str = "")
        : info(move(info_)), payload(move(pay)), s(move(str)) {}
};
struct Thread {
    vector<Ref<Closure>> argStack; // der a izq
    // vector<Ref<Closure>> retStack; // no se comparten resultados
    vector<Ref<Closure>> updStack;
    Ref<Closure> current;
};
static string Show(const Ref<Closure>& c) {
    if (!c) return "⊥";
    switch (c->info->tag) {
        case Tag::CON: return string("\"") + c->s + "\"";
        case Tag::FUN: return c->info->name;
        case Tag::PAP: return string("PAP(") + c->payload[0]->info->name + ")";
        case Tag::AP:  return "(" + Show(c->payload[0]) + " @ " + Show(c->payload[1]) + ")";
        case Tag::THUNK: return "THUNK";
        case Tag::IND: return "IND->" + Show(c->payload[0]);
    }
    return "?";
}
static inline Ref<Closure> mkClosure(Ref<Info> info, vector<Ref<Closure>> payload = {}, string s = "") {
    return make_shared<Closure>(move(info), move(payload), move(s));
}
static inline Ref<Info> mkInfo(Tag tag, int arity, function<void(Thread&, Ref<Closure>)> entry, const char* name) {
    return make_shared<Info>(Info{tag, arity, move(entry), name});
}
static Ref<Info> FUN_I, FUN_K, FUN_S, FUN_B, FUN_C, FUN_Sp, FUN_Cp, FUN_Bs, FUN_Succ, FUN_Dup;
static Ref<Info> INFO_THUNK, INFO_PAP, INFO_AP, INFO_CON, INFO_IND;
static void stgEnter(Thread& T, Ref<Closure> cl);
static inline void pushArg(Thread& T, const Ref<Closure>& a) { T.argStack.push_back(a); }
static inline Ref<Closure> popArg(Thread& T) {
    auto a = T.argStack.back(); T.argStack.pop_back(); return a;
}
static Ref<Closure> mkPAP(Ref<Closure> fun, const vector<Ref<Closure>>& args) {
    vector<Ref<Closure>> pay; pay.reserve(1 + args.size());
    pay.push_back(fun);
    for (auto& a : args) pay.push_back(a);
    return mkClosure(INFO_PAP, move(pay));
}
static Ref<Closure> mkAP(Ref<Closure> f, Ref<Closure> x) {
    return mkClosure(INFO_AP, {f, x});
}
static Ref<Closure> mkStr(const string& s) {
    return mkClosure(INFO_CON, {}, s);
}
static void papEntry(Thread& T, Ref<Closure> cl) {
    auto& pay = cl->payload;
    Ref<Closure> fun = pay[0];
    for (size_t i = 1; i < pay.size(); ++i) pushArg(T, pay[i]);
    stgEnter(T, fun);
}
static void apEntry(Thread& T, Ref<Closure> cl) {
    auto f = cl->payload[0];
    auto x = cl->payload[1];
    pushArg(T, x);
    stgEnter(T, f);
}
static void thunkEntry(Thread& T, Ref<Closure> cl) {
    T.updStack.push_back(cl);
    stgEnter(T, cl->payload[0]);
    auto res = T.current;
    cl->info = INFO_IND;
    cl->payload = {res};
    T.updStack.pop_back();
    T.current = res;
}
static void indEntry(Thread& T, Ref<Closure> cl) {
    stgEnter(T, cl->payload[0]);
}
static void conEntry(Thread& T, Ref<Closure> cl) {
    T.current = cl;
}
static void stgEnter(Thread& T, Ref<Closure> cl) {
    for (;;) {
#ifdef DEBUG
        cerr << Show(cl) << endl;
#endif
        switch (cl->info->tag) {
            case Tag::IND: { cl = cl->payload[0]; continue; }
            case Tag::CON: {
                if (!T.argStack.empty()) { // sigue concatenando si sobran args
                    auto arg = popArg(T);
                    stgEnter(T, arg);
                    auto A = T.current;
                    if (A->info->tag != Tag::CON) {
                        T.current = mkAP(cl, A);
                        return;
                    }
                    cl = mkStr(cl->s + A->s);
                    continue;
                }
                T.current = cl;
                return;
            }
            case Tag::AP: {
                auto f = cl->payload[0];
                auto x = cl->payload[1];
                if (f->info->tag == Tag::CON) {
                    stgEnter(T, x); // fuerza cadena
                    auto X = T.current;
                    if (X->info->tag == Tag::CON) {
                        cl = mkStr(f->s + X->s);
                        continue;
                    }
                    T.current = mkAP(f, X);
                    return;
                }
                pushArg(T, x);
                cl = f;
                continue;
            }
            case Tag::PAP: {
                auto& pay = cl->payload;
                Ref<Closure> fun = pay[0];
                for (size_t i = 1; i < pay.size(); ++i) pushArg(T, pay[i]);
                cl = fun;
                continue;
            }
            case Tag::THUNK: {
                T.updStack.push_back(cl);
                auto body = cl->payload[0];
                stgEnter(T, body);
                auto res = T.current;
                cl->info = INFO_IND;
                cl->payload = {res};
                T.updStack.pop_back();
                cl = res;
                continue;
            }
            case Tag::FUN: {
                int need = cl->info->arity;
                int have = (int)T.argStack.size();
                if (have < need) {
                    vector<Ref<Closure>> tmp;
                    while (!T.argStack.empty()) tmp.push_back(popArg(T));
                    reverse(tmp.begin(), tmp.end());
                    T.current = mkPAP(cl, tmp);
                    return;
                }
                cl->info->entry(T, cl);
                cl = T.current;
                continue;
            }
        }
    }
}
static inline Ref<Closure> app(Ref<Closure> f, Ref<Closure> x) { return mkAP(move(f), move(x)); }
static inline Ref<Closure> operator-(Ref<Closure> f, Ref<Closure> x) { return app(f,x); }
static void entry_I(Thread& T, Ref<Closure> f) {
    auto x = popArg(T);
    T.current = x;
}
static void entry_K(Thread& T, Ref<Closure> f) {
    auto x = popArg(T);
    auto y = popArg(T);
    (void)y;
    T.current = x;
}
static void entry_S(Thread& T, Ref<Closure> f) {
    auto x = popArg(T);
    auto y = popArg(T);
    auto z = popArg(T);
    T.current = x-z-(y-z);
}
static void entry_B(Thread& T, Ref<Closure> f) {
    auto F = popArg(T);
    auto g = popArg(T);
    auto x = popArg(T);
    T.current = F-(g-x);
}
static void entry_C(Thread& T, Ref<Closure> f) {
    auto F = popArg(T);
    auto g = popArg(T);
    auto x = popArg(T);
    T.current = F-x-g;
}
static void entry_Sp(Thread& T, Ref<Closure> f) {
    auto c = popArg(T);
    auto F = popArg(T);
    auto g = popArg(T);
    auto x = popArg(T);
    T.current = c-(F-x)-(g-x);
}
static void entry_Cp(Thread& T, Ref<Closure> f) {
    auto c = popArg(T);
    auto F = popArg(T);
    auto g = popArg(T);
    auto x = popArg(T);
    T.current = c-(F-x)-g;
}
static void entry_Bs(Thread& T, Ref<Closure> f) {
    auto c = popArg(T);
    auto F = popArg(T);
    auto g = popArg(T);
    auto x = popArg(T);
    T.current = c-(F-(g-x));
}
static void entry_Succ(Thread& T, Ref<Closure> f) {
    auto n = popArg(T);
    auto F = popArg(T);
    auto x = popArg(T);
    T.current = F-(n-F-x);
}
static void entry_Dup(Thread& T, Ref<Closure> f) {
    auto n = popArg(T);
    auto F = popArg(T);
    auto x = popArg(T);
    T.current = n-F-(n-F-x);
}
static Ref<Info> FUN_Concat;
static void entry_Concat(Thread& T, Ref<Closure> f) {
    auto b = popArg(T);
    auto a = popArg(T);
    stgEnter(T, a); // forzamiento de concatenación (debería ser bajo demanda)
    auto A = T.current;
    stgEnter(T, b);
    auto B = T.current;
    if (A->info->tag == Tag::CON && B->info->tag == Tag::CON) {
        T.current = mkStr(A->s + B->s);
    } else {
        T.current = app(app(mkClosure(FUN_Concat), A), B);
    }
}
#define MK_BN(N) \
    static void entry_Bn##N(Thread& T, Ref<Closure> f) { \
        auto F = popArg(T); \
        auto G = popArg(T); \
        std::vector<Ref<Closure>> xs; xs.reserve(N); \
        for (int i=0;i<N;i++) xs.push_back(popArg(T)); \
        auto arg = popArg(T); \
        Ref<Closure> gx = G; \
        for (auto& xi : xs) gx = app(gx, xi); \
        gx = app(gx, arg); \
        T.current = app(F, gx); \
    } \
    static Ref<Info> FUN_Bn##N = mkInfo(Tag::FUN, (N)+2, entry_Bn##N, "B"#N); \
    static Ref<Closure> Bn##N##_ = mkClosure(FUN_Bn##N);
#define MK_CN(N) \
    static void entry_Cn##N(Thread& T, Ref<Closure> f) { \
        auto F = popArg(T); \
        auto G = popArg(T); \
        std::vector<Ref<Closure>> xs; xs.reserve(N); \
        for (int i=0;i<N;i++) xs.push_back(popArg(T)); \
        auto arg = popArg(T); \
        Ref<Closure> fx = F; \
        for (auto& xi : xs) fx = app(fx, xi); \
        fx = app(fx, arg); \
        T.current = app(fx, G); \
    } \
    static Ref<Info> FUN_Cn##N = mkInfo(Tag::FUN, (N)+2, entry_Cn##N, "C"#N); \
    static Ref<Closure> Cn##N##_ = mkClosure(FUN_Cn##N);
#define MK_SN(N) \
    static void entry_Sn##N(Thread& T, Ref<Closure> f) { \
        auto F = popArg(T); \
        auto G = popArg(T); \
        std::vector<Ref<Closure>> xs; xs.reserve(N); \
        for (int i=0;i<N;i++) xs.push_back(popArg(T)); \
        auto arg = popArg(T); \
        Ref<Closure> fx = F; \
        for (auto& xi : xs) fx = app(fx, xi); \
        fx = app(fx, arg); \
        Ref<Closure> gx = G; \
        for (auto& xi : xs) gx = app(gx, xi); \
        gx = app(gx, arg); \
        T.current = app(fx, gx); \
    } \
    static Ref<Info> FUN_Sn##N = mkInfo(Tag::FUN, (N)+2, entry_Sn##N, "S"#N); \
    static Ref<Closure> Sn##N##_ = mkClosure(FUN_Sn##N);
static void initInfos() {
    INFO_PAP   = mkInfo(Tag::PAP,   0, papEntry, "PAP");
    INFO_AP    = mkInfo(Tag::AP,    0, apEntry,  "AP");
    INFO_THUNK = mkInfo(Tag::THUNK, 0, thunkEntry, "THUNK");
    INFO_IND   = mkInfo(Tag::IND,   0, indEntry, "IND");
    INFO_CON   = mkInfo(Tag::CON,   0, conEntry, "STR");
    FUN_I   = mkInfo(Tag::FUN, 1, entry_I,   "I");
    FUN_K   = mkInfo(Tag::FUN, 2, entry_K,   "K");
    FUN_S   = mkInfo(Tag::FUN, 3, entry_S,   "S");
    FUN_B   = mkInfo(Tag::FUN, 3, entry_B,   "B");
    FUN_C   = mkInfo(Tag::FUN, 3, entry_C,   "C");
    FUN_Sp  = mkInfo(Tag::FUN, 4, entry_Sp,  "Sp");
    FUN_Cp  = mkInfo(Tag::FUN, 4, entry_Cp,  "Cp");
    FUN_Bs  = mkInfo(Tag::FUN, 4, entry_Bs,  "Bs");
    FUN_Succ= mkInfo(Tag::FUN, 3, entry_Succ,"Succ");
    FUN_Dup = mkInfo(Tag::FUN, 3, entry_Dup, "Dup");
    FUN_Concat = mkInfo(Tag::FUN, 2, entry_Concat, "++");
}
static Ref<Closure> I_, K_, S_, B_, C_, Sp_, Cp_, Bs_, Succ_, Dup_, Concat_, Z_;
static void initGlobals() {
    I_ = mkClosure(FUN_I);
    K_ = mkClosure(FUN_K);
    S_ = mkClosure(FUN_S);
    B_ = mkClosure(FUN_B);
    C_ = mkClosure(FUN_C);
    Sp_ = mkClosure(FUN_Sp);
    Cp_ = mkClosure(FUN_Cp);
    Bs_ = mkClosure(FUN_Bs);
    Succ_ = mkClosure(FUN_Succ);
    Dup_  = mkClosure(FUN_Dup);
    Concat_ = mkClosure(FUN_Concat);
    Z_ = K_-I_;
}
static int log2_ull(unsigned long long n) {
#if defined(__GNUC__)
    return 63 - __builtin_clzll(n);
#else
    int r = -1; while (n) { n >>= 1; ++r; } return r;
#endif
}
static Ref<Closure> ParseNum(unsigned long long n) {
    if (n == 0) return Z_;
    Ref<Closure> N = Z_;
    for (int i = log2_ull(n); i >= 0; --i) {
        N = Dup_-N;
        if ((n & (1ull << i)) != 0) N = Succ_-N;
    }
    return N;
}
static Ref<Closure> ParseArg(const string& arg) {
    if (arg.empty()) throw runtime_error("[ERROR (Runtime)]: Argumento vacío no permitido");
    if (arg == "true")  return K_;
    if (arg == "false") return Z_;
    if (arg[0] == '/') return mkStr(arg.substr(1));
    try {
        size_t idx = 0;
        unsigned long long n = stoull(arg, &idx, 10);
        if (idx == arg.size()) return ParseNum(n);
    } catch (...) {}
    throw runtime_error("[ERROR (Runtime)]: Cada argumento debe ser /string, true, false o un número natural");
}
static Ref<Closure> Evaluate(Ref<Closure> expr) {
    Thread T;
    T.current = expr;
    stgEnter(T, T.current);
    return T.current;
}
|]

-- |Genera información adicional en C++ dependiente del programa
pregen :: Expr -> String
pregen e = concat $ HS.toList (aux e)
    where aux :: Expr -> HS.HashSet String -- Genera las constantes de los Bn/Cn/Sn usados
          aux (BnComb n) = HS.singleton $ "MK_BN(" ++ show n ++ ");\n"
          aux (CnComb n) = HS.singleton $ "MK_CN(" ++ show n ++ ");\n"
          aux (SnComb n) = HS.singleton $ "MK_SN(" ++ show n ++ ");\n"
          aux (EApp lhs rhs) = HS.union (aux lhs) (aux rhs)
          aux _ = HS.empty

-- |Genera la expresión en C++ a la cual se transforma la expresión SKI
gen :: Expr -> String
gen SComb      = "S_"
gen KComb      = "K_"
gen IComb      = "I_"
gen BComb      = "B_"
gen CComb      = "C_"
gen S'Comb     = "Sp_"
gen C'Comb     = "Cp_"
gen BsComb     = "Bs_"
gen (BnComb n) = "Bn" ++ show n ++ "_"
gen (CnComb n) = "Cn" ++ show n ++ "_"
gen (SnComb n) = "Sn" ++ show n ++ "_"
gen (Str s)    = "St(" ++ strLiteral s ++ ")"
gen (EApp lhs rhs@(EApp _ _)) = gen lhs ++ "-(" ++ gen rhs ++ ")"
gen (EApp lhs rhs) = gen lhs ++ "-" ++ gen rhs

-- |Escapa cadenas de SKI en un literal de C++
strLiteral :: String -> String
strLiteral s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c    = [c]
