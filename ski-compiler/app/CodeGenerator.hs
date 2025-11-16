module CodeGenerator (
    codeGen
) where

import Parser
import Data.List(intercalate)

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
       , "        result = Apply(result, Value(std::string(argv[i])));"
       , "    }"
       , "    std::cout << pretty(result) << \"\\n\";"
       , "    return 0;"
       , "}"
       ]

-- |Preludio del código a generar
prelude :: [String]
prelude =
  [ "#include <iostream>"
  , "#include <string>"
  , "#include <functional>"
  , "#include <variant>"
  , "#include <stdexcept>"
  , ""
  , "struct Value;"
  , "using Func = std::function<Value(const Value&)>;"
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
  , "Value Apply(const Value& f, const Value& x) {"
  , "    if (f.isString()) {"
  , "        // Cadena izquierda, concatena derecha"
  , "        if (x.isString()) {"
  , "            return Value(f.asString() + x.asString());"
  , "        } else if (x.isFunc()) {"
  , "            Value rhs = x.asFunc()(Value(\"\"));"
  , "            if (rhs.isString()) {"
  , "                return Value(f.asString() + rhs.asString());"
  , "            }"
  , "            throw std::runtime_error(\"Apply: se aplico cadena a un resultado no-cadena\");"
  , "        } else {"
  , "            throw std::runtime_error(\"Apply: se aplico cadena a no-cadena\");"
  , "        }"
  , "    }"
  , "    if (!f.isFunc()) {"
  , "        throw std::runtime_error(\"Apply: se aplico un valor no-funcion\");"
  , "    }"
  , "    return f.asFunc()(x);"
  , "}"
  , ""
  , "// I x = x"
  , "Value I = Value(Func{"
  , "    [](const Value& x) -> Value {"
  , "        return x;"
  , "    }"
  , "});"
  , ""
  , "// K x y = x"
  , "Value K = Value(Func{"
  , "    [](const Value& x) -> Value {"
  , "        return Value(Func{"
  , "            [x](const Value& /*y*/) -> Value {"
  , "                return x;"
  , "            }"
  , "        });"
  , "    }"
  , "});"
  , ""
  , "// S f g x = f x (g x)"
  , "Value S = Value(Func{"
  , "    [](const Value& f) -> Value {"
  , "        return Value(Func{"
  , "            [f](const Value& g) -> Value {"
  , "                return Value(Func{"
  , "                    [f,g](const Value& x) -> Value {"
  , "                        Value fx = Apply(f, x);"
  , "                        Value gx = Apply(g, x);"
  , "                        return Apply(fx, gx);"
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
gen :: Expr -> String
gen SComb      = "S"
gen KComb      = "K"
gen IComb      = "I"
gen (Str s)    = "Value(" ++ strLiteral s ++ ")"
gen (EApp l r) = "Apply(" ++ gen l ++ ", " ++ gen r ++ ")"

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
