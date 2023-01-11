# SugarFox
Un transpilador para **Visual Foxpro 9.0** escrito en el mismo **Visual Foxpro 9.0**

# Introducción

Un transpilador es una especie de compilador que toma código fuente escrito en un languaje de programación "A" y en lugar de generar código máquina o emsamblador genera código fuente para otro lenguaje de programación existente "B". Este tipo de artefactos también se conoce como compiladores source to source ya que tanto el código de entrada como el código de salida generanlemente son de alto nivel.

**SugarFox** es un lenguaje de programación que en lugar de ejecutar programas lo que hace es generar un código **Visual Foxpro** como salida que posteriormente será ejecutado por el runtime de VFP.

# Sintaxis

La sintaxis de SugarFox es muy similar **Visual Foxpro** de manera que cualquier programador Fox pueda familiarizarse con el lenguaje en poco tiempo. Sin embargo hay que tener en cuenta que:

1. SugarFox no es Visual Foxpro
2. SugarFox es sensible a las mayúsculas
3. Las variables obligatoriamente deben declararse antes de usarse.

A continuación se detallan cada una de las características de SugarFox:

## Comentarios

SugarFox hereda el formato de Python para los comentarios:

```Python
# Este es un comentario de una linea

"""
Este es un comentario 
de múltiples
lineas
"""
```

## Declaración de variables

Del mismo modo que en Visual Foxpro, las variables pueden ser locales, privadas o públicas.


```Lua
local a = 10
local b = 20
local c = a + b
```

_NOTA: las variables en esta versión se declaran en forma lineal e independiente, es decir, no es posible crear un conjunto de variables separadas por coma, en una segunda versión incluiré esta característica._

## Literales

Los literales en SugarFox son similares a los de Fox salvo por algunas mejoras.


### Cadenas de Texto
```lua
# Cadenas de texto sin escape de caracteres
print("Este es un string de comillas dobles")
print('Este es un string de comillas simples')

# Cadenas de texto con escape de caracteres
print("Esta es una linea \n y esta es otra linea.")
print(`Esta es una cadena cruda que no \n escapa \t \r caracteres.`)
```

### Números
En SugarFox solo existe un tipo de dato para los números y es el `number`.

```Python
print(1)        # enteros
print(1985.37)  # notación con decimales

print(123_456_78) # este también es un número
```

### Boolean

```Ruby
print(true)
print(false)
```

### Arrays

```Lua
local dias = ["Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"]
print(dias[1]) # Lunes
```

### Diccionarios
```Lua
local precios = {
    "Aceite de Oliva": 3.57,
    "Gel de Ducha 1L": 3.98,
    "Leche desnatada 1L": 0.70
}
print(precios["Leche desnatada"]) # 0.70
```

### For Loop

```Ruby
# Imprime los números del 1 al 10
for i in 10
    print(i)
end

# Imprime el índice y el valor de un array
local dias = ["Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"]
for i, v in dias
    print(i, v)
end

# Imprime las claves y los valores de un diccionario
local datos = {
    "nombre": "Pedro",
    "apellido": "Peres",
    "edad": 37
}

for k, v in datos
    print(k, v)
end

```

### While Loop

```Ruby
local i = 1

while i < 10
    print(i)
    i += 1
end
```

### Do While Loop

```Ruby
local i = 1

do
    print(i)
    i += 1
while i < 10
```

## Condicional If

```Ruby
local verdad = true

if verdad
    print("Verdad")
else
    print("Falso")
end
```

## Funciones
```Ruby
print(Sumar(5, 10))

func Sumar(x, y)
    return x + y
end
```

## Guardias
```Swift
local loDatos = {
    "nombre": "Pedro",
    "apellido": "Peres",
    "edad": 37
}

imprimirDatos(loDatos)

"""
Función que imprime los datos de un diccionario
NOTA: el diccionario debe tener una clave llamada direccion
      o de lo contrario no se imprime nada.
      para esta validación usamos un guardia.
"""
func imprimirDatos(toDatos)
    guard toDatos["direccion"] else
        print("Faltan datos")
        return
    end

    # Imprimir los datos
    for k, v in toDatos
        print(k, v)
    end
end
```

## Enumerables
```Swift
enum Colores
    ROJO,       # toma 1 por defecto a menos que se especifique un valor.
    VERDE = 15, # explícitamente se ha especificado el valor 15.
    AMARILLO    # su valor automáticamente es 16
end

local miColor = Colores.AMARILLO
print(miColor) # 16
```
