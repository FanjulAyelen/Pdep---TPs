%colecciona(Coleccionista, nueva o cambiada(numero, paquete o persona)).
colecciona(andy, nueva(2, 1)). 
colecciona(andy, nueva(4, 1)).
colecciona(andy, nueva(7, 2)).
colecciona(andy, nueva(6, 2)).
colecciona(andy, nueva(8, 3)).
colecciona(andy, nueva(1, 3)).
colecciona(andy, nueva(3, 3)).
colecciona(andy, nueva(5, 3)).
colecciona(flor, nueva(5, 1)). 
colecciona(flor, nueva(5, 2)).
colecciona(toto, nueva(1, 1)). 
colecciona(bobby, nueva(3, 1)). 
colecciona(bobby, nueva(5, 1)). 
colecciona(bobby, nueva(7, 2)).
colecciona(lala, nueva(3, 1)).
colecciona(lala, nueva(7, 1)).
colecciona(lala, nueva(1, 1)).
colecciona(pablito, cambiada(1, lala)).
colecciona(lala, cambiada(5, pablito)).
colecciona(flor, cambiada(4, andy)).
colecciona(flor, cambiada(7, andy)).
colecciona(andy, cambiada(1, flor)).
colecciona(flor, cambiada(2, bobby)).
colecciona(bobby, cambiada(1, flor)).
colecciona(bobby, cambiada(4, flor)).
colecciona(bobby, cambiada(6, flor)).
colecciona(pablito, cambiada(2, toto)).
colecciona(toto, cambiada(6, pablito)).

/*----- PUNTO 1 -----*/

tieneFigurita(Persona, Figurita):-
    colecciona(Persona, Forma),
    esFormaDeObtener(Forma, Figurita).

esFormaDeObtener(nueva(Figurita, _), Figurita).
esFormaDeObtener(cambiada(Figurita, _), Figurita).

/*----- PUNTO 2 -----*/

figuritaRepetida(Persona, Figurita) :-
    tieneFigurita(Persona, Figurita),
    repetida(Persona, Figurita).

repetida(Persona, Figurita):-
    findall(Figurita, tieneFigurita(Persona, Figurita), Repeticiones),
    length(Repeticiones, Total),
    Total > 1.

/*----- PUNTO 3 -----*/

esRara(Figurita):-
    figurita(Figurita, _),
    forall(colecciona(_, nueva(Figurita, Paquete)), Paquete > 2).

esRara(Figurita):-
    losColeccionistas(TotalDeColeccionista),
    tienenLaFigurita(Figurita, TienenFigurita),
    TienenFigurita < TotalDeColeccionista / 2,
    forall(tieneFigurita(Persona, Figurita), not(figuritaRepetida(Persona, Figurita))).

losColeccionistas(Total):-
    findall(Persona, colecciona(Persona, _), Lista),
    totalSinRepetidos(Lista, Total).

tienenLaFigurita(Figurita, Total):-
    figurita(Figurita, _),
    findall(Persona, tieneFigurita(Persona, Figurita), Lista),
    totalSinRepetidos(Lista, Total).

totalSinRepetidos(Lista, Total):-
    list_to_set(Lista, NuevaLista),
    length(NuevaLista, Total).

/*-----PUNTO 4-----*/

figurita(1, basica([kitty, keroppi])). 
figurita(2, brillante(kitty)).
figurita(3, brillante(myMelody)).
figurita(4, basica([])).
figurita(5, rompecabeza(restaurante)).
figurita(6, rompecabeza(restaurante)).
figurita(7, rompecabeza(restaurante)).
figurita(8, basica([kitty, keroppi, cinnamoroll, kuromi, pompompurin, badtzMaru, littleTwinStars, gudetama, myMelody])).
/*Agregamos la figuritaa 9 ya que el enunciados señala que se puede agregar información de más figuritas a la base de conocimientos más allá de la indicada en el punto.*/
figurita(9, brillante(cinnamoroll)). %figurita agregada

%popularidad(nombre, popularidad) 
popularidad(kitty, 5).
popularidad(cinnamoroll, 4).
popularidad(badtzMaru, 2).
popularidad(keroppi, 3).
popularidad(pompompurin, 4).
popularidad(gudetama, 1).
popularidad(myMelody, 3).
popularidad(littleTwinStars, 2).
popularidad(kuromi, 5).

/*----- PUNTO 5 -----*/

esValiosa(Figurita):-
    esRara(Figurita).

esValiosa(Figurita):-
    figurita(Figurita, TipoFigurita),
    atractivo(TipoFigurita, Nivel),
    Nivel > 7.

atractivo(brillante(Personaje), Nivel):-
    popularidad(Personaje, Popularidad),
    Nivel is 5 * Popularidad.

atractivo(basica(ListaDePersonajes), Nivel):-
    listaDePopularidad(ListaDePersonajes, ListaDePopularidad),
    sumlist(ListaDePopularidad, Nivel). 

atractivo(rompecabeza(Imagen), 0):-
    totalDePiezas(Imagen, Total),
    Total > 2.

atractivo(rompecabeza(Imagen), 2):-
    totalDePiezas(Imagen, Total),
    Total =< 2.

buscarPopularidad(Personaje, ListaDePersonajes, Popularidad):-
    member(Personaje, ListaDePersonajes), 
    popularidad(Personaje, Popularidad).

listaDePopularidad(ListaDePersonajes, ListaDePopularidad):-
    findall(Popularidad, buscarPopularidad(_, ListaDePersonajes, Popularidad), ListaDePopularidad).

totalDePiezas(Imagen, Total):-
    figurita(_, rompecabeza(Imagen)),
    findall(Pieza, figurita(Pieza, rompecabeza(Imagen)), PiezasDelRompecabezas),
    length(PiezasDelRompecabezas, Total).

/*-----PUNTO 6-----*/

imagenMasAtractiva(Persona, FiguritaMasAtractiva):-
    tieneFigurita(Persona, FiguritaMasAtractiva),
    forall((tieneFigurita(Persona, Figurita), FiguritaMasAtractiva \= Figurita), esMasAtractiva(FiguritaMasAtractiva, Figurita)).

buscarAtractivo(Persona, Figurita, Atractivo):-
    tieneFigurita(Persona, Figurita), 
    figurita(Figurita, TipoFigurita),
    atractivo(TipoFigurita, Atractivo).

esMasAtractiva(Figurita, OtraFigurita):-
    buscarAtractivo(_, Figurita, MayorAtractivo),
    buscarAtractivo(_, OtraFigurita, MenorAtractivo),
    MayorAtractivo > MenorAtractivo.

/*-----PUNTO 7-----*/

interesante(Persona, paquete(Figuritas), Total):-
    interesEnLasFiguritas(Persona, Figuritas, Total).

interesante(Persona, canje(Persona, _, _, FiguritasRecibidas), Total):-
    interesEnLasFiguritas(Persona, FiguritasRecibidas, Total).

interesEnLasFiguritas(Persona, Figuritas, Total):-
    colecciona(Persona, _), 
    figuritasFaltantes(Persona, Figuritas, Faltantes), 
    atractivoTotal(Faltantes, Interes),
    sumaPlus(Faltantes, Numero),
    Total is Numero + Interes.

buscarAtractivo(ListaDeFiguritas, Atractivo):-
    member(Figurita, ListaDeFiguritas), 
    atractivo(Figurita, Atractivo).

atractivoTotal(ListaDeFiguritas, NivelDeAtractivo):-
    findall(Atractivo, buscarAtractivo(ListaDeFiguritas, Atractivo), ListaDeAtractivos),
    sumlist(ListaDeAtractivos, NivelDeAtractivo).

figuritasFaltantes(Persona, ListaDeFiguritas, Faltantes):-
	findall(Figurita, noTieneFiguritas(Persona, Figurita, ListaDeFiguritas), Lista),
    list_to_set(Lista, Faltantes).

noTieneFiguritas(Persona, Figurita, ListaDeFiguritas):-
    figurita(Figurita, _), %cambiado
    member(Figurita, ListaDeFiguritas), 
    not(tieneFigurita(Persona, Figurita)).

hayRaras(ListaDeFiguritas):-
    member(Figurita, ListaDeFiguritas),
    esRara(Figurita).

sumaPlus(Faltantes, 20):-
    hayRaras(Faltantes).
sumaPlus(Faltantes, 0):-
    not(hayRaras(Faltantes)).

/*-----PUNTO 8-----*/

esValido(paquete(Figuritas)):-
    forall(member(Figurita, Figuritas), figurita(Figurita, _)).

esValido(canje(Persona, FiguritasDadas, Canjeador, FiguritasRecibidas)):-
    tieneLasFiguritas(Persona, FiguritasDadas),
    tieneLasFiguritas(Canjeador, FiguritasRecibidas).

tieneLasFiguritas(Persona, Figuritas):-
    colecciona(Persona, _),
    forall(member(Figurita, Figuritas), tieneFigurita(Persona, Figurita)).

/*-----PUNTO 9-----*/

haceNegocio(canje(_, FiguritasDadas, _, FiguritasRecibidas)):- 
    hayValiosa(FiguritasRecibidas),
    not(hayValiosa(FiguritasDadas)).

hayValiosa(Figuritas):-
    member(Figurita, Figuritas),
    esValiosa(Figurita).

/*-----PUNTO 10 -----*/

album(Album):- 
    findall(Figurita, figurita(Figurita, _), Album). 

necesitaConUrgencia(Persona, Figurita):-
    album(Album),
    noTieneFiguritas(Persona, Figurita, Album),
    cumpleCondicionDeUrgencia(Persona, Figurita, Album).

cumpleCondicionDeUrgencia(Persona, Figurita, Album):-
    tieneTodasLasDemas(Persona, Figurita, Album).

cumpleCondicionDeUrgencia(Persona, Figurita, _):-
    parteDeRompecabezas(Persona, Figurita, _).

parteDeRompecabezas(Persona, Figurita, OtraFigurita):-
    figurita(Figurita, rompecabeza(Imagen)),
    figurita(OtraFigurita, rompecabeza(Imagen)),
    Figurita \= OtraFigurita,
    tieneFigurita(Persona, OtraFigurita).
    
tieneTodasLasDemas(Persona, FiguritaQueFalta, Album):-
    forall(noEsLaMisma(FiguritaQueFalta, OtraFigurita, Album), tieneFigurita(Persona, OtraFigurita)).

noEsLaMisma(Figurita, OtraFigurita, Lista):-
    member(OtraFigurita, Lista), 
    OtraFigurita \= Figurita.

/*-----PUNTO 11-----*/

esUnaAmenaza(Persona, Canjes):-
    haceAlgunNegocio(Persona, Canjes),
    saleGanandoSiempre(Persona, Canjes).

haceAlgunNegocio(Persona, Canjes):-
    member(canje(Persona, FiguritasDadas, Canjeador, FiguritasRecibidas), Canjes),
    haceNegocio(canje(Persona, FiguritasDadas, Canjeador, FiguritasRecibidas)).

saleGanando(Persona, canje(Persona, FiguritasDadas, _, FiguritasRecibidas)):-
    interesEnLasFiguritas(Persona, FiguritasDadas, InteresDeLasQuePerdio),
    interesEnLasFiguritas(Persona, FiguritasRecibidas, InteresDeLasQueRecibio),
    InteresDeLasQueRecibio > InteresDeLasQuePerdio.

saleGanandoSiempre(Persona, Canjes):-
    forall(member(UnCanje, Canjes), saleGanando(Persona, UnCanje)).

/*-----PUNTO 12-----*/

canjeValido(Persona, canje(Persona, FiguritasDeLaPersona, Canjeador, FiguritasCanjeador)):-
    colecciona(Persona, _),
    esValido(canje(Persona, FiguritasDeLaPersona, Canjeador, FiguritasCanjeador)),
    hayRepetidas(Persona, FiguritasDeLaPersona),
    estiloDeColeccionismo(Canjeador, FiguritasDeLaPersona, FiguritasCanjeador).

estiloDeColeccionismo(OtraPersona, FiguritasDeLaPersona, _):-
    clasico(OtraPersona, FiguritasDeLaPersona). 

estiloDeColeccionismo(OtraPersona, _, FiguritasCanjeador):-
    descartador(OtraPersona, FiguritasCanjeador).

estiloDeColeccionismo(OtraPersona, FiguritasDeLaPersona, _):-
    cazafortunas(OtraPersona, FiguritasDeLaPersona). 

estiloDeColeccionismo(OtraPersona, FiguritasDeLaPersona, _):-
    urgida(OtraPersona, FiguritasDeLaPersona).

hayRepetidas(Persona, Figuritas):-
    member(Figurita, Figuritas),
    figuritaRepetida(Persona, Figurita).

clasico(Persona, FiguritasRecibidas):-
    forall(member(Figurita, FiguritasRecibidas), not(tieneFigurita(Persona, Figurita))).

descartador(Persona, FiguritasDadas):-
    forall(member(Figurita, FiguritasDadas), figuritaRepetida(Persona, Figurita)).

cazafortunas(_, FiguritasRecibidas):-
    hayValiosa(FiguritasRecibidas).

urgida(Persona, FiguritasRecibidas):-
    hayAlgunaUrgente(Persona, FiguritasRecibidas).

hayAlgunaUrgente(Persona, Figuritas):-
    member(Figurita, Figuritas),
    necesitaConUrgencia(Persona, Figurita).