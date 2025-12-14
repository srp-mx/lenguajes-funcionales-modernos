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
codeGenInfo = "Generador de código SKI estricto"

-- |Genera el programa objetivo en C++
codeGen :: Expr -> String
codeGen expr =
    let oexpr = optimizeExpr expr
    in prelude
       ++ pregen oexpr
       ++ "Value ProgramExpr = " ++ gen oexpr ++ ";"
       ++ [r|
int main(int argc, char* argv[]) {
    Value result = ProgramExpr;
    for (int i = 1; i < argc; ++i) {
        result = result-ParseArg(argv[i]);
    }
    if (!result.isString()) return 1;
    std::cout << pretty(result) << "\n";
    return 0;
}|]

-- |Preludio del código a generar
prelude :: String
prelude = [r|// CÓDIGO GENERADO CON srp-mx/lenguajes-funcionales-modernos
#include <iostream>
#include <string>
#include <functional>
#include <variant>
#include <stdexcept>
struct Value;
using Func = std::function<Value(const Value&)>;
struct Value {
    std::variant<std::string, Func> data;
    Value() : data(std::string{}) {}
    Value(std::string s) : data(std::move(s)) {}
    Value(Func f) : data(std::move(f)) {}
    bool isString() const { return std::holds_alternative<std::string>(data); }
    bool isFunc()   const { return std::holds_alternative<Func>(data); }
    const std::string& asString() const { return std::get<std::string>(data); }
    const Func&        asFunc()   const { return std::get<Func>(data); }
};
#define St(x) Value(x)
Value Apply(const Value& f, const Value& x) {
    if (f.isString()) {
        if (x.isString()) {
            return Value(f.asString() + x.asString());
        } else if (x.isFunc()) {
            Value rhs = x.asFunc()(Value(""));
            if (rhs.isString()) {
                return Value(f.asString() + rhs.asString());
            }
            throw std::runtime_error("Apply: se aplico cadena a un resultado no-cadena");
        } else {
            throw std::runtime_error("Apply: se aplico cadena a no-cadena");
        }
    }
    if (!f.isFunc()) {
        throw std::runtime_error("Apply: se aplico un valor no-funcion");
    }
    return f.asFunc()(x);
}
inline static Value operator-(const Value& f, const Value& x) { return Apply(f,x); }
// Bs c f g x = c (f (g x))
Value Bs = Value(Func{
    [](const Value& c) -> Value {
        return Value(Func{
            [c](const Value& f) -> Value {
                return Value(Func{
                    [c,f](const Value& g) -> Value {
                        return Value(Func{
                            [c,f,g](const Value& x) -> Value {
                                Value gx = Apply(g,x);
                                Value fgx = Apply(f,gx);
                                return Apply(c, fgx);
                            }
                        });
                    }
                });
            }
        });
    }
});
// C' c f g x = c (f x) g
Value Cp = Value(Func{
    [](const Value& c) -> Value {
        return Value(Func{
            [c](const Value& f) -> Value {
                return Value(Func{
                    [c,f](const Value& g) -> Value {
                        return Value(Func{
                            [c,f,g](const Value& x) -> Value {
                                Value fx = Apply(f,x);
                                Value cfx = Apply(c,fx);
                                return Apply(cfx, g);
                            }
                        });
                    }
                });
            }
        });
    }
});
// S' c f g x = c (f x) (g x)
Value Sp = Value(Func{
    [](const Value& c) -> Value {
        return Value(Func{
            [c](const Value& f) -> Value {
                return Value(Func{
                    [c,f](const Value& g) -> Value {
                        return Value(Func{
                            [c,f,g](const Value& x) -> Value {
                                Value fx = Apply(f,x);
                                Value gx = Apply(g,x);
                                Value cfx = Apply(c,fx);
                                return Apply(cfx, gx);
                            }
                        });
                    }
                });
            }
        });
    }
});
// C f g x = f x g
Value C = Value(Func{
    [](const Value& f) -> Value {
        return Value(Func{
            [f](const Value& g) -> Value {
                return Value(Func{
                    [f,g](const Value& x) -> Value {
                        Value fx = Apply(f, x);
                        return Apply(fx, g);
                    }
                });
            }
        });
    }
});
// B f g x = f (g x)
Value B = Value(Func{
    [](const Value& f) -> Value {
        return Value(Func{
            [f](const Value& g) -> Value {
                return Value(Func{
                    [f,g](const Value& x) -> Value {
                        Value gx = Apply(g, x);
                        return Apply(f, gx);
                    }
                });
            }
        });
    }
});
// I x = x
Value I = Value(Func{
    [](const Value& x) -> Value {
        return x;
    }
});
// K x y = x
Value K = Value(Func{
    [](const Value& x) -> Value {
        return Value(Func{
            [x](const Value& /*y*/) -> Value {
                return x;
            }
        });
    }
});
// S f g x = f x (g x)
Value S = Value(Func{
    [](const Value& f) -> Value {
        return Value(Func{
            [f](const Value& g) -> Value {
                return Value(Func{
                    [f,g](const Value& x) -> Value {
                        Value fx = Apply(f, x);
                        Value gx = Apply(g, x);
                        return Apply(fx, gx);
                    }
                });
            }
        });
    }
});
Value applyMany(Value f, const std::vector<Value>& args) {
    Value r = f;
    for (auto& a : args) {
        r = Apply(r, a);
    }
    return r;
}
Value collectN(int k, std::function<Value(const std::vector<Value>&)> finalize) {
    struct Collector {
        int remaining;
        std::vector<Value> acc;
        std::function<Value(const std::vector<Value>&)> done;
        Value step(const Value& v) {
            acc.push_back(v);
            if (--remaining == 0) {
                return done(acc);
            }
            Collector next{remaining, acc, done};
            return Value(Func{
                [next](const Value& u) mutable {
                    Collector c = next;
                    return c.step(u);
                }
            });
        }
    };
    Collector c{k, {}, finalize};
    return Value(Func{[c](const Value& a1) mutable { return c.step(a1); }});
}
// Bn: f g x1..xn arg = f (g x1..xn arg)
Value makeBn(int n) {
    return Value(Func{[n](const Value& f) {
        return Value(Func{[n,f](const Value& g) {
            return collectN(n + 1, [f, g](const std::vector<Value>& xs) -> Value {
                Value gx = applyMany(g, xs);
                return Apply(f, gx);
            });
        }});
    }});
}
// Cn: f g x1..xn arg = f x1..xn arg g
Value makeCn(int n) {
    return Value(Func{[n](const Value& f) {
        return Value(Func{[n,f](const Value& g) {
            return collectN(n + 1, [f, g](const std::vector<Value>& xs) -> Value {
                Value fx = applyMany(f, xs);
                return Apply(fx, g);
            });
        }});
    }});
}
// Sn: f g x1..xn arg = (f x1..xn arg) (g x1..xn arg)
Value makeSn(int n) {
    return Value(Func{[n](const Value& f) {
        return Value(Func{[n,f](const Value& g) {
            return collectN(n + 1, [f, g](const std::vector<Value>& xs) -> Value {
                Value fx = applyMany(f, xs);
                Value gx = applyMany(g, xs);
                return Apply(fx, gx);
            });
        }});
    }});
}
#define BN_MK(N) Value B##N = makeBn(N-1);
#define CN_MK(N) Value C##N = makeCn(N-1);
#define SN_MK(N) Value S##N = makeSn(N-1);
std::string pretty(const Value& v) {
    if (v.isString()) return v.asString();
    return "";
}
static Value ParseNum(unsigned long long n) {
    return Value(Func{[n](const Value& f) {
        return Value(Func{[n,f](const Value& x) {
            Value acc = x;
            for (unsigned long long i = 0; i < n; ++i) acc = Apply(f, acc);
            return acc;
        }});
    }});
}
static Value ParseArg(const std::string& arg) {
    if (arg.empty())
        throw std::runtime_error("[ERROR]: Argumento vacío no permitido");
    if (arg == "true")
        return K;
    if (arg == "false")
        return K-I;
    if (arg[0] == '/')
        return Value(arg.substr(1));
    try {
        size_t idx = 0;
        unsigned long long n = std::stoull(arg, &idx, 10);
        if (idx == arg.size())
            return ParseNum(n);
    } catch (...) {}
    throw std::runtime_error("[ERROR]: Cada argumento debe ser /string, true, false o un número natural");
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
gen SComb      = "S"
gen KComb      = "K"
gen IComb      = "I"
gen BComb      = "B"
gen CComb      = "C"
gen S'Comb     = "Sp"
gen C'Comb     = "Cp"
gen BsComb     = "Bs"
gen (BnComb n) = "B" ++ show n
gen (CnComb n) = "C" ++ show n
gen (SnComb n) = "S" ++ show n
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
