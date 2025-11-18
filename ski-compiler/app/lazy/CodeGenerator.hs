module CodeGenerator (
    codeGen,
    codeGenInfo
) where

import Expr
import Data.List (intercalate)

-- |Información de la generación de código
codeGenInfo :: String
codeGenInfo = "Generador de código SKI con evaluación perezosa (thunks)"

-- |Genera el programa objetivo en C++
codeGen :: Expr -> String
codeGen expr =
  intercalate "\n" $
    prelude
    ++ [ ""
       , "// Código generado"
       , "Value ProgramExpr = " ++ gen expr ++ ";"
       , ""
       , "int main(int argc, char* argv[]) {"
       , "    Value result = ProgramExpr;"
       , "    for (int i = 1; i < argc; ++i) {"
       , "        result = Apply(result, thunk(Value(std::string(argv[i]))));"
       , "    }"
       , "    std::cout << pretty(result) << \"\\n\";"
       , "    return 0;"
       , "}"
       ]

-- |Preludio del código a generar (perezoso con thunks)
prelude :: [String]
prelude =
  [ "#include <iostream>"
  , "#include <string>"
  , "#include <functional>"
  , "#include <variant>"
  , "#include <stdexcept>"
  , ""
  , "struct Value;"
  , "struct Thunk { std::function<Value()> force; };"
  , "using Func = std::function<Value(const Thunk&)>;"
  , ""
  , "struct Value {"
  , "    std::variant<std::string, Func> data;"
  , "    Value() : data(std::string{}) {}"
  , "    Value(std::string s) : data(std::move(s)) {}"
  , "    Value(Func f) : data(std::move(f)) {}"
  , "    bool isString() const { return std::holds_alternative<std::string>(data); }"
  , "    bool isFunc()   const { return std::holds_alternative<Func>(data); }"
  , "    const std::string& asString() const { return std::get<std::string>(data); }"
  , "    const Func&        asFunc()   const { return std::get<Func>(data); }"
  , "};"
  , ""
  , "inline Thunk thunk(Value v) { return Thunk{ [v]{ return v; } }; }"
  , "inline Thunk thunkFrom(const Thunk& t) { return Thunk{ [t]{ return t.force(); } }; }"
  , ""
  , "Value Apply(const Value& f, const Thunk& x) {"
  , "    if (f.isString()) {"
  , "        // Cadena izquierda: concatena con el resultado perezoso de x"
  , "        Value xv = x.force();"
  , "        if (xv.isString()) {"
  , "            return Value(f.asString() + xv.asString());"
  , "        } else if (xv.isFunc()) {"
  , "            // Intentamos obtener cadena aplicando a \"\" de forma perezosa"
  , "            Value rhs = xv.asFunc()(thunk(Value(\"\")));"
  , "            if (rhs.isString()) {"
  , "                return Value(f.asString() + rhs.asString());"
  , "            }"
  , "            throw std::runtime_error(\"Apply: se aplicó cadena a un resultado no-cadena\");"
  , "        } else {"
  , "            throw std::runtime_error(\"Apply: se aplicó cadena a no-cadena\");"
  , "        }"
  , "    }"
  , "    if (!f.isFunc()) {"
  , "        throw std::runtime_error(\"Apply: se aplicó un valor no-función\");"
  , "    }"
  , "    return f.asFunc()(x);"
  , "}"
  , ""
  , "// I x = x"
  , "Value I = Value(Func{"
  , "    [](const Thunk& x) -> Value {"
  , "        return x.force();"
  , "    }"
  , "});"
  , ""
  , "// K x y = x (no fuerza y)"
  , "Value K = Value(Func{"
  , "    [](const Thunk& x) -> Value {"
  , "        Value vx = x.force();"
  , "        return Value(Func{"
  , "            [vx](const Thunk& /*y*/) -> Value {"
  , "                return vx;"
  , "            }"
  , "        });"
  , "    }"
  , "});"
  , ""
  , "// S f g x = f x (g x), construyendo thunk para (g x)"
  , "Value S = Value(Func{"
  , "    [](const Thunk& f) -> Value {"
  , "        Value vf = f.force();"
  , "        return Value(Func{"
  , "            [vf](const Thunk& g) -> Value {"
  , "                Value vg = g.force();"
  , "                return Value(Func{"
  , "                    [vf, vg](const Thunk& x) -> Value {"
  , "                        // f x"
  , "                        Value fx = Apply(vf, x);"
  , "                        // thunk para (g x) sin forzar"
  , "                        Thunk gxThunk{ [vg, x]{ return Apply(vg, x); } };"
  , "                        // aplicar (f x) a (g x) perezosamente"
  , "                        return Apply(fx, gxThunk);"
  , "                    }"
  , "                });"
  , "            }"
  , "        });"
  , "    }"
  , "});"
  , ""
  , "std::string pretty(const Value& v) {"
  , "    if (v.isString()) return \"\\\"\" + v.asString() + \"\\\"\";"
  , "    return \"<funcion>\";"
  , "}"
  ]

-- |Genera la expresión en C++ a la cual se transforma la expresión SKI
--  con thunks perezosos
gen :: Expr -> String
gen SComb = "S"
gen KComb = "K"
gen IComb = "I"
gen (Str s) = "Value(" ++ strLiteral s ++ ")"
gen (EApp l r) = "Apply(" ++ gen l ++ ", thunk(" ++ gen r ++ "))"

-- |Escapa cadenas de SKI en un literal de C++
strLiteral :: String -> String
strLiteral s = "\"" ++ concatMap esc s ++ "\""
    where esc '"'  = "\\\""
          esc '\\' = "\\\\"
          esc '\n' = "\\n"
          esc '\r' = "\\r"
          esc '\t' = "\\t"
          esc c    = [c]
