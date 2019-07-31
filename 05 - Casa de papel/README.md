Casa de papel
================
Elio Campitelli

Primero leemos los datos

``` r
library(data.table)
library(magrittr)
library(tidytext)

cdp <- fread("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-31/la_casa_de_papel.csv")

# Creo un thread para twittear esto
thread <- spindler::thread$new()

thread$add_post("Hola, en este #DatosDeMiercoles la idea es jugar con los subtítulos de la serie 'La Casa de Papel'. Es la primera vez que trabajo con datos de texto, así que lo que sigue es un mamarracho horrendo. Pero divertido! #Rstats #RstatsES")$
  add_post("Podría contar palabras, hacer análisis de sentimientos (algo que nunca me convenció; mucho menos en esppañol), pero para hacerlo más divertido, decidí crear un generador de fraces. Adelante!")
```

``` r
str(cdp)
```

    ## Classes 'data.table' and 'data.frame':   15222 obs. of  6 variables:
    ##  $ ID_epi        : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ tiempo_entrada: chr  "00:00:06.520" "00:00:07.840" "00:00:08.920" "00:00:25.160" ...
    ##  $ tiempo_salida : chr  "00:00:07.760" "00:00:08.840" "00:00:09.760" "00:00:26.360" ...
    ##  $ texto         : chr  "(HOMBRE) ¡Quieto o disparo!" "(Disparo)" "(MUJER) ¡No!" "\"\"Me llamo Tokio." ...
    ##  $ episodio      : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ temporada     : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

Nunca trabajé con textos pero sí leí varios blogs al respecto. Se puede
analizar frecuencia de palabras, o [logs odds
ratios](https://juliasilge.com/blog/introducing-tidylo/). Hay mucho de
análisis de sentimiento que nunca me convenció mucho, y menos en
español. Así que ante mi total ignorancia sobre el tema, voy a hacer
algo muy a lo bruto. Voy a tratar de construir un generador de
“oraciones” a partir de estos datos.

El plan es calcular la probabilidad de que a una palabra le siga otra.
Entonces a partir de una palabra inicial, seleccionar la siguiente
palabra de forma aleatoria pero según la probabilidad observada.

Primero lo de siempre: “tokenizar” el texto.

``` r
palabras <- cdp %>% 
  unnest_tokens(palabra, texto) %>% 
  # unnest_tokens(palabra, texto) %>% 
  as.data.table() 
head(palabras)
```

    ##    ID_epi tiempo_entrada tiempo_salida episodio temporada palabra
    ## 1:      1   00:00:06.520  00:00:07.760        1         1  hombre
    ## 2:      1   00:00:06.520  00:00:07.760        1         1  quieto
    ## 3:      1   00:00:06.520  00:00:07.760        1         1       o
    ## 4:      1   00:00:06.520  00:00:07.760        1         1 disparo
    ## 5:      2   00:00:07.840  00:00:08.840        1         1 disparo
    ## 6:      3   00:00:08.920  00:00:09.760        1         1   mujer

Y ahora calculo la frecuencia.

``` r
frecuencias <- palabras %>%
  .[, palabra_sig := shift(palabra, type = "lead"), 
    by = .(ID_epi, episodio, temporada)] %>%   # agrego una columna para la palabra siguiente.
  na.omit() %>% 
  .[, n_palabra := .N, by = .(palabra)] %>%               # número de veces que aparece la palabra (para normalizar)
  .[, .(n_palabra_sig = .N, n_palabra = n_palabra[1]), by = .(palabra, palabra_sig)] %>% 
  .[, prob := n_palabra_sig/n_palabra] %>%                
  .[n_palabra > 5] %>%                                    # me quedo con las palabras que aparecen más de 5 veces
  .[order(-prob)]

head(frecuencias)
```

    ##    palabra palabra_sig n_palabra_sig n_palabra prob
    ## 1:    tono          de            24        24    1
    ## 2: opinión     pública             8         8    1
    ## 3: aseguro         que             7         7    1
    ## 4:   creía         que            11        11    1
    ## 5: remedio         que             7         7    1
    ## 6:  partir          de             6         6    1

Se ve que hay algunos 2-gramas (pares de palabras) que aparecen siempre
juntos. A la palabra “tono” **siempre** le sigue la palabra “de”. Es
razonable. Cabe aclarar que esto está hecho muy a lo bruto.

Ahora me hago una función que elije la palabra siguiente a partir de la
palabra anterior.

``` r
palabra_siguiente <- function(palabra_anterior) {
  # Si la palabra está entre las palabras observadas
  if (palabra_anterior %in% frecuencias$palabra) {
    palabra_siguiente <- frecuencias[palabra == palabra_anterior] %>% 
      .[, sample(palabra_sig, size = 1, prob = prob)]     # Samplea 1 palabra sigiente usando la probabilidad calculada arriba
  } else {
    # Si la palabra no aparece...
    palabra_siguiente <- frecuencias %>% 
      .[, sample(palabra, 1)]          # Elegir cualquiera aleatoriametne pero según su frecuencia. 
  }
  
  return(palabra_siguiente)
}
```

Probemos. (Seteo la semilla para que sea reproducible.)

``` r
set.seed(42)
palabra_siguiente("hola")
```

    ## [1] "hola"

Mh.. una mala jugada del azar\!

Ahora lo único que hay que hacer es iterar. Voy a armarme una función
que genere una oración de `N` palabras.

``` r
oracion <- function(primera_palabra, N = 3) {
  oracion <- vector(mode = "character", length = N)
  oracion[1] <- primera_palabra
  for (p in seq_len(N)[-1]) {
    oracion[p] <- palabra_siguiente(oracion[p-1])
  }
  return(paste0(oracion, collapse = " "))
}
```

A ver, probemos.

``` r
(hola <- oracion("hola", 7))
```

    ## [1] "hola denver no mencionó único que matar"

🤷. Alguien que vio la serie me podrá decir si le parece algo que podrían
decir.

``` r
(en <- oracion("en", 5))
```

    ## [1] "en esa esa particularidad está"

JAJAJAJAJAJAJAJAAAAAA\!\!\!\!\!\!

``` r
(pero <- oracion("pero", 5))
```

    ## [1] "pero jamás se quedó allí"

¿Hay aire acondicionado en el hospital?

Si somos ambiciosos con la longitud de la
    oración:

``` r
(ellos <- oracion("ellos", 20))
```

    ## [1] "ellos van a regatear seis siete ocho rehenes dejarán el tocadiscos un jodido profesor y tengo que estos momentos he"

Mh… ya se vuelve poesía moderna.

Elegir palabras así tiene el problema de que da muchas cosas sin
sentido. Una forma de “arreglarlo” podria ser considerar ngrams más
largos. En el caso anterior esencialmente estaba seleccionando a partir
de los pares de palabras más probables. ¿Qué pasa si en vez de 2, uso 3?

Parecido a lo anterior, divido en series de 3 palabras y me fijo la
frecuencia de 3-gram para cada palabra con la que comienza.

``` r
palabra_n <- function(texto, n) {
  vapply(strsplit(texto, " "), function(x) {
    if (n[1] == "ultima") {
      x[length(x)]
    } else {
      paste0(x[n], collapse = " ")
    }
  } , "a")
}

freq_ngrams <- cdp %>% 
  .[, unnest_tokens(.SD, ngram, texto, token = "ngrams", n = 3), by = .(ID_epi, temporada, episodio)] %>% 
  na.omit() %>% 
  .[, primera_palabra := palabra_n(ngram, 1)] %>% 
  .[, .(n_ngram = .N), by = .(ngram, primera_palabra)] 

freq_ngrams[primera_palabra == "yo"] %>% 
  .[order(-n_ngram)]
```

    ##              ngram primera_palabra n_ngram
    ##   1:   yo creo que              yo      14
    ##   2:     yo no soy              yo      13
    ##   3:     yo qué sé              yo      10
    ##   4:     yo sé que              yo      10
    ##   5: yo también me              yo       5
    ##  ---                                      
    ## 303:  yo le enseño              yo       1
    ## 304:    yo he sido              yo       1
    ## 305:      yo sí os              yo       1
    ## 306:  yo no miento              yo       1
    ## 307:  yo no confío              yo       1

Ahí tenemos que, por ejemplo, si empezamos con la palabra “yo”, lo más
apropiado sería continuar con “creo que”. Entonces armemos algo
parecido. En este caso, si no encuentra la palabra con la empieza,
directamente termina la oración (esto no es ideal, pero en fin). Además,
estoy tokenizando dentro de cada ID de subtítulo para que no se mezclen
las líneas de diálogo.

``` r
ngram <- function(primera_palabra) {
  primera_palabra_in <- primera_palabra
  
  # Si la palabra está entre las palabras observadas
  if (primera_palabra_in %in% freq_ngrams$primera_palabra) {
    ngram <- freq_ngrams[primera_palabra == primera_palabra_in] %>% 
      .[, sample(ngram, 1, prob = n_ngram)]
  } else {
    # Si la freq_ngrams no aparece...
    ngram <- ""
  }
  return(ngram)
}


oracion_ngram <- function(primera_palabra, N = 3) {
  oracion <- vector(mode = "list")
  oracion[[1]] <- primera_palabra
  
  for (n in seq_len(N)+1) {
    prox_ngram <- ngram(primera_palabra)
    
    if (prox_ngram != "") {
      oracion[[n]] <- palabra_n(prox_ngram, 2:3)
    } else {
      break
    }
    
    primera_palabra <- palabra_n(prox_ngram, "ultima")
  }
  return(paste0(oracion, collapse = " "))
}
```

Probemos nuestra nueva Casa de Papel, entonces.

``` r
set.seed(42)
(ella <- oracion_ngram("ella", 6))
```

    ## [1] "ella disparó primero tenemos que te gusta vivir no eras rehén ya no"

Ok, parece que la gramática mejoró un poco, pero tampoco tiene demasiado
sentido.

``` r
(es <- oracion_ngram("es", 7))
```

    ## [1] "es así manda a los flancos para hacer un vaso de protesta contra las rocas"

Mhh.. no.

``` r
(cuando <- oracion_ngram("cuándo", 4))
```

    ## [1] "cuándo van a parecer una zorra egoísta y obsesiva"

Volento, pero al menos es una oración con
    sentido\!

``` r
(siempre <- oracion_ngram("siempre", 6))
```

    ## [1] "siempre y cuando estamos a hablarme así de la vida no les va"

No mejoró mucho. Creo que el problema es que al usar 3-grams lo que en
realidad tengo que hacer es predecir la tercer palabra a partir de las 2
anteriores. El problema es que rápidamente me quedo sin combinaciones
observadas\!
