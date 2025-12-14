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
```bash
cabal run subs-compiler-lazy -- -o cuenta-veinte < ../program-samples/subs/twenty.subs
```

Y puedes correr tu programa al ir al directorio generado `compiler-build`,
donde se encontrará tu programa, al cual puedes pasarle datos como argumentos
de la línea de comandos.

```
Las cadenas se pasan comenzando con '/', por ejemplo ./cuenta-diez /a /b

Los números deben ser naturales, por ejemplo ./cuenta-diez 2 /a /b

Se puede pasar textualmente true y false, por ejemplo ./cuenta-diez false /a /b
```

Si el programa (con argumentos) es irreducible pero no se reduce a una cadena,
se termina la ejecución con estatus de error, sin imprimir nada.

Los lenguajes disponibles son todos azúcar sintáctica del lenguaje del cálculo
de combinadores SKI con cadenas. Éstos son:

* `ski`: El cálculo SKI
* `lambda`: El cálculo lambda
* `subs`: El cálculo lambda con expresiones nombradas a nivel global,
          posiblemente con argumentos

Los modos disponibles son:

* `strict`: Evaluación estricta al compilar a un *runtime* de C++
* `lazy`: Evaluación perezosa al compilar a un *runtime* de C++

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

### Pruebas

Dentro de `ski-compiler/` se encuentra el archivo de pruebas `tests.py`, el
cual hay que ejecutarlo ahí mismo. Prueba todos los modos con todos los
programas de ejemplo.

Nótese que se espera que `twenty.subs` falle en modo estricto, pero funcione
en modo lazy, pues implementa el combinador `Y`.

## Reporte

Su código fuente se encuentra bajo `reporte/`, el cual tiene su propio archivo
`README.md`.
