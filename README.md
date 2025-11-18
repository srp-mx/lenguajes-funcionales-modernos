# Lenguajes Funcionales Modernos

## Compiladores

### Uso

Ve a `ski-compiler` y compila todos los compiladores con
```bash
cabal build
```

Corre algún compilador con
```bash
cabal run [leng]-compiler-[modo] -- -o mi-programa < mi-programa.src
```
donde `[leng]` es el nombre del lenguaje que quieres, `[modo]` el modo de
generación de código, `mi-programa` el nombre de tu ejecutable compilado y
`mi-programa.src` el código fuente de tu programa.

Por ejemplo,
```bash
cabal run subs-compiler-strict -- -o cuenta-diez < ../program-samples/subs/numbers.subs
```

Y puedes correr tu programa al ir al directorio generado `compiler-build`,
donde se encontrará tu programa, al cual puedes pasarle datos como argumentos
de la línea de comandos.

Los lenguajes disponibles son todos azúcar sintáctica del lenguaje del cálculo
de combinadores SKI con cadenas. Éstos son:

* `ski`: El cálculo SKI
* `lambda`: El cálculo lambda
* `subs`: El cálculo lambda con expresiones nombradas a nivel global,
          posiblemente con argumentos

Los modos disponibles son:

* `strict`: Evaluación estricta al compilar a un *runtime* de C++

### Lenguajes

#### ski

```
<Expresion> ::= <Atomo> { <Atomo> }*
<Atomo> ::= S | s | K | k | I | i | <String> | ( <Expresion> )
<String> ::= una cadena multilínea entre "

<Comentario> ::= una línea que inicia en //
<MComentario> ::= texto encerrado entre /* y */
<Espacio> ::= espacio en blanco
```

#### lambda

```
<Expresion> ::= <Identificador>
             |  <String>
             |  ( <Expresion> <Expresion> )
             |  [ <Identificador> <Expresion> ]
<String> ::= una cadena multilínea entre "
<Identificador> ::= comienza con una letra o . y puede continuar con letras,
                    dígitos, ', ?, _, -, .

<Comentario> ::= una línea que inicia en //
<MComentario> ::= texto encerrado entre /* y */
<Espacio> ::= espacio en blanco
```

#### subs

```
<Programa> ::= <Declaracion> { ; { ; }* <Declaracion> }* { ; }*
<Declaracion> ::= <Identificador> { <Identificador> }* = <Expresion>

<Expresion> ::= <Identificador>
             |  <String>
             |  ( <Expresion> <Expresion> )
             |  [ <Identificador> <Expresion> ]
<String> ::= una cadena multilínea entre "
<Identificador> ::= comienza con una letra o . y puede continuar con letras,
                    dígitos, ', ?, _, -, .

<Comentario> ::= una línea que inicia en //
<MComentario> ::= texto encerrado entre /* y */
<Espacio> ::= espacio en blanco
```

### Ejemplos

Ejemplos de programa por lenguaje se encuentran en `program-samples/`.

## Reporte

Su código fuente se encuentra bajo `reporte/`, el cual tiene su propio archivo
`README.md`.
