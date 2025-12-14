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
       ++ "const static Ref<Node> program = " ++ gen oexpr ++ ";"
       ++ [r|
int main(int argc, char** argv) {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    Ref<Node> p = program;
    for (int i = 1; i < argc; ++i) {
        p = p-ParseArg(argv[i]);
    }
#ifdef TRACE
    auto trace = Trace(p, traceSteps);
    for (auto& step : trace) {
        cout << Show(step) << endl;
    }
#else
    Ref<Node> result = Evaluate(p);
    if (auto s = dynamic_pointer_cast<Str>(result)) {
        cout << s->s << "\n";
    }
#endif
    return 0;
}|]

-- |Preludio del código a generar
prelude :: String
prelude = [r|// CÓDIGO GENERADO CON srp-mx/lenguajes-funcionales-modernos
#include <bits/stdc++.h>
#ifdef TRACE
constexpr int traceSteps = (TRACE);
#endif
using namespace std;
struct Node; struct Closure; struct Str; struct App;
#define mk make_shared
template<typename T> using Ref = shared_ptr<T>;
inline static Ref<Node> operator-(const Ref<Node>& a, const Ref<Node>& b) {
    return static_pointer_cast<Node>(mk<App>(a, b));
}
struct Parenty {
    Ref<Parenty> P; Ref<Node> Value; int Depth;
    Parenty(Ref<Node> value, Ref<Parenty> p = nullptr)
        : P(move(p)), Value(move(value)), Depth(P ? P->Depth + 1 : 0) {}
    vector<Ref<Node>> Collect() const {
        vector<Ref<Node>> outputs(Depth + 1);
        const Parenty* cur = this;
        while (cur) {
            outputs[cur->Depth] = cur->Value;
            cur = cur->P.get();
        }
        return outputs;
    }
};
struct Node : enable_shared_from_this<Node> {
    virtual ~Node() = default;
    virtual Ref<Node> StepOnce() = 0;
    virtual string Show_() const = 0;
};
inline static string Show(const Ref<Node>& n) {
    if (!n) return "⊥";
    return n->Show_();
}
struct Str : Node {
    string s;
    explicit Str(string s_) : s(move(s_)) {}
    Ref<Node> StepOnce() override { return shared_from_this(); }
    string Show_() const override { return string("\"") + s + "\""; }
};
inline static Ref<Node> operator+(const Ref<Str>& a, const Ref<Str>& b) {
    return mk<Str>(a->s + b->s);
}
struct Closure : Node {
    const Ref<Parenty> captures;
    explicit Closure(Ref<Parenty> caps = nullptr) : captures(move(caps)) {}
    virtual string name() const = 0;
    virtual Ref<Node> Apply(const Ref<Node>& arg) = 0;
    Ref<Node> StepOnce() override { return shared_from_this(); }
    string Show_() const override {
        string s = name();
        if (!captures) return s;
        for (const auto& cap : captures->Collect()) {
            s += "[" + Show(cap) + "]";
        }
        return s;
    }
};
struct App : Node {
    Ref<Node> left, right;
    App(Ref<Node> l, Ref<Node> r) : left(move(l)), right(move(r)) {}
    Ref<Node> app(const Ref<Node>& nextL, const Ref<Node>& nextR) {
        if (nextL.get() == left.get() && nextR.get() == right.get())
            return shared_from_this();
        return nextL-nextR;
    }
    Ref<Node> StepOnce() override {
        if (auto f = dynamic_pointer_cast<Closure>(left)) {
            return f->Apply(right);
        }
        if (auto l = dynamic_pointer_cast<App>(left)) {
            return app(l->StepOnce(), right);
        }
        if (auto lstr = dynamic_pointer_cast<Str>(left)) {
            if (auto rstr = dynamic_pointer_cast<Str>(right)) {
                return lstr + rstr;
            }
            if (auto ra = dynamic_pointer_cast<App>(right)) {
                if (auto rl = dynamic_pointer_cast<Str>(ra->left)) {
                    return (lstr + rl)-(ra->right);
                }
                return app(left, right->StepOnce());
            }
            return app(left, right->StepOnce());
        }
        return app(left, right->StepOnce());
    }
    string Show_() const override {
        return "(" + Show(left) + " @ " + Show(right) + ")";
    }
};
template<typename Tag, size_t Arity>
struct ClosureImp : Closure {
    static_assert(Arity >= 1, "La aridad de un combinador debe ser al menos 1");
    explicit ClosureImp(Ref<Parenty> caps = nullptr) : Closure(move(caps)) {}
    inline string cname() const;
    string name() const override { return cname(); }
    inline Ref<Node> OnSaturation(const Ref<Node>& arg) const;
    Ref<Node> Apply(const Ref<Node>& arg) override {
        if constexpr (Arity == 1) {
            return OnSaturation(arg);
        } else {
            if (!captures || captures->Depth < Arity-2) {
                return mk<ClosureImp<Tag,Arity>>(mk<Parenty>(arg,captures));
            }
            return OnSaturation(arg);
        }
    }
};
template<typename Iter>
static inline Ref<Node> ApplyRange(Ref<Node> f, Iter begin, Iter end) {
    Ref<Node> t = move(f);
    for (Iter it = begin; it != end; ++it) {
        t = t - *it;
    }
    return t;
}
#define COMBINATOR(NAME, ARITY) \
    struct NAME##_Tag_ {}; \
    using NAME = ClosureImp<NAME##_Tag_, ARITY>; \
    template<> inline string NAME::cname() const { return #NAME; } \
    template<> inline Ref<Node> NAME::OnSaturation(const Ref<Node>& arg) const
struct Bn_Tag_ {};
#define BN_MK(N) \
    using Bn##N = ClosureImp<Bn_Tag_, N+2>; \
    template<> inline string Bn##N::cname() const { return "B" #N; } \
    template<> inline Ref<Node> Bn##N::OnSaturation(const Ref<Node>& arg) const {\
        auto caps = this->captures->Collect(); \
        Ref<Node> f = caps[0], g = caps[1]; \
        Ref<Node> gx = ApplyRange(g, caps.begin() + 2, caps.end()); \
        gx = gx - arg; \
        return f - gx; \
    } \
    const static Ref<Bn##N> Bn##N##_ = mk<Bn##N>();
struct Cn_Tag_ {};
#define CN_MK(N) \
    using Cn##N = ClosureImp<Cn_Tag_, N+2>; \
    template<> inline string Cn##N::cname() const { return "C" #N; } \
    template<> inline Ref<Node> Cn##N::OnSaturation(const Ref<Node>& arg) const {\
        auto caps = this->captures->Collect(); \
        Ref<Node> f = caps[0], g = caps[1]; \
        Ref<Node> fx = ApplyRange(f, caps.begin() + 2, caps.end()); \
        fx = fx - arg; \
        return fx - g; \
    } \
    const static Ref<Cn##N> Cn##N##_ = mk<Cn##N>();
struct Sn_Tag_ {};
#define SN_MK(N) \
    using Sn##N = ClosureImp<Sn_Tag_, N+2>; \
    template<> inline string Sn##N::cname() const { return "S" #N; } \
    template<> inline Ref<Node> Sn##N::OnSaturation(const Ref<Node>& arg) const {\
        auto caps = this->captures->Collect(); \
        Ref<Node> f = caps[0], g = caps[1]; \
        Ref<Node> fx = ApplyRange(f, caps.begin() + 2, caps.end()); \
        fx = fx - arg; \
        Ref<Node> gx = ApplyRange(g, caps.begin() + 2, caps.end()); \
        gx = gx - arg; \
        return fx - gx; \
    } \
    const static Ref<Sn##N> Sn##N##_ = mk<Sn##N>();
COMBINATOR(I, 1) { return arg; }
COMBINATOR (K, 2) { return captures->Value; }
COMBINATOR(S, 3) {
    auto caps = this->captures->Collect();
    Ref<Node> x = caps[0], y = caps[1], z = arg;
    return (x-z)-(y-z);
}
COMBINATOR(B, 3) {
    auto caps = this->captures->Collect();
    Ref<Node> f = caps[0], g = caps[1], x = arg;
    return f-(g-x);
}
COMBINATOR(C, 3) {
    auto caps = this->captures->Collect();
    Ref<Node> f = caps[0], g = caps[1], x = arg;
    return f-x-g;
}
COMBINATOR(Sp, 4) {
    auto caps = this->captures->Collect();
    Ref<Node> c = caps[0], f = caps[1], g = caps[2], x = arg;
    return c-(f-x)-(g-x);
}
COMBINATOR(Cp, 4) {
    auto caps = this->captures->Collect();
    Ref<Node> c = caps[0], f = caps[1], g = caps[2], x = arg;
    return c-(f-x)-g;
}
COMBINATOR(Bs, 4) {
    auto caps = this->captures->Collect();
    Ref<Node> c = caps[0], f = caps[1], g = caps[2], x = arg;
    return c-(f-(g-x));
}
COMBINATOR(Succ, 3) {
    auto caps = this->captures->Collect();
    Ref<Node> n = caps[0], f = caps[1], x = arg;
    return f-(n-f-x);
}
COMBINATOR(Dup, 3) {
    auto caps = this->captures->Collect();
    Ref<Node> n = caps[0], f = caps[1], x = arg;
    return n-f-(n-f-x);
}
static bool haltEval(const Ref<Node>& next, const Ref<Node>& current) {
    return next.get() == current.get();
}
static Ref<Node> Evaluate(Ref<Node> expr) {
    Ref<Node> current = move(expr);
    for (;;) {
        Ref<Node> next = current->StepOnce();
        if (haltEval(next, current)) break;
        current = move(next);
    }
    return current;
}
static vector<Ref<Node>> Trace(Ref<Node> expr, int maxSteps) {
    vector<Ref<Node>> out;
    Ref<Node> current = move(expr);
    out.push_back(current);
    for (int i = 0; i < maxSteps; ++i) {
        Ref<Node> next = current->StepOnce();
        if (haltEval(next, current)) break;
        out.push_back(next);
        current = move(next);
    }
    return out;
}
const static Ref<I> I_ = mk<I>();
const static Ref<K> K_ = mk<K>();
const static Ref<S> S_ = mk<S>();
const static Ref<B> B_ = mk<B>();
const static Ref<C> C_ = mk<C>();
const static Ref<Sp> Sp_ = mk<Sp>();
const static Ref<Cp> Cp_ = mk<Cp>();
const static Ref<Bs> Bs_ = mk<Bs>();
const static Ref<Succ> Succ_ = mk<Succ>();
const static Ref<Dup> Dup_ = mk<Dup>();
const static Ref<Node> Z = K_-I_;
static int log2_ull(unsigned long long n) {
#if defined(__GNUC__)
    return 63 - __builtin_clzll(n);
#else
    int r = -1;
    while (n) { n >>= 1; ++r; }
    return r;
#endif
}
static Ref<Node> ParseNum(unsigned long long n) {
    if (n == 0) return Z;
    Ref<Node> N = Z;
    for (int i = log2_ull(n); i >= 0; --i) {
        N = Dup_-N;
        if ((n & (1ull << i)) != 0)
            N = Succ_-N;
    }
    return N;
}
static Ref<Node> ParseArg(const string& arg) {
    if (arg == "")
        throw runtime_error("[ERROR (Runtime)]: Argumento vacío no permitido");
    if (arg == "true")
        return K_;
    if (arg == "false")
        return Z;
    if (!arg.empty() && arg[0] == '/')
        return mk<Str>(arg.substr(1));
    try {
        size_t idx = 0;
        unsigned long long n = stoull(arg, &idx, 10);
        if (idx == arg.size())
            return ParseNum(n);
    } catch (...) {}
    throw runtime_error("[ERROR (Runtime)]: Cada argumento debe ser /string, true, false o un número natural");
}
static inline Ref<Node> St(const string& s) {
    return mk<Str>(s);
}
|]

-- |Genera información adicional en C++ dependiente del programa
pregen :: Expr -> String
pregen e = concat $ HS.toList (aux e)
    where aux :: Expr -> HS.HashSet String -- Genera las constantes de los Bn/Cn/Sn usados
          aux (BnComb n) = HS.singleton $ "BN_MK(" ++ show n ++ ");\n"
          aux (CnComb n) = HS.singleton $ "CN_MK(" ++ show n ++ ");\n"
          aux (SnComb n) = HS.singleton $ "SN_MK(" ++ show n ++ ");\n"
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
