# Reporte

Requisitos LaTeX
----------------

Si no tienes instalado los requisitos de LaTeX, ejecuta
```
./install-latex
```

o bien

```
python install-latex
```

Es posible que durante la instalación requieras aceptar los paquetes y/o correr
una GUI de instalación manualmente.

Uso
---

Los archivos para el LaTeX del reporte se encuentran bajo el subdirectorio
`/latex`.

Dentro del directorio de LaTeX del reporte se encuentra un subarchivo por
sección. Estos son los archivos `.tex` dentro del directorio `subs`.

En el directorio de LaTeX vienen las siguientes herramientas:
* Se compila todo con `./build`.
* Se compila todo con salida de la terminal reducida con `./build-silent`.
* Se limpian archivos auxiliares que genera LaTeX con `./clean`.

Estos archivos también los puedes ejecutar con python, por ejemplo:
```
python build
```

El archivo `.pdf` se llamará por defecto `main.pdf`.

Además, en cada directorio de LaTeX hay otro llamado `img` donde se colocarán
imágenes. Estas imágenes se pueden referir en el código por su nombre
directamente, por ejemplo: `\includegraphics{imagen.png}` si existe
`img/imagen.png`.

Ya vienen preincluidos varios paquetes y comandos, puedes revisarlos y
modificarlos en `util/incluye.sty` y `util/comandos.sty`.

Puedes agregar referencias en el archivo `referencias.bib` en el formato de
*biblatex*, y citarlas en tus archivos `.tex` con `\parencite{tu-referencia}`.
