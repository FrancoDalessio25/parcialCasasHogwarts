% Parcial Casas Hogwarts


sangre(harry,mestriza).
sangre(draco,pura).
sangre(hermione,impura).

caracteristicas(harry,corajudo).
caracteristicas(harry,amistoso).
caracteristicas(harry,orgulloso).
caracteristicas(harry,inteligente).

caracteristicas(draco,inteligente).
caracteristicas(draco,orgulloso).

caracteristicas(hermione,inteligente).
caracteristicas(hermione,orgulloso).
caracteristicas(hermione,responsable).

odiariaIrA(harry,slytherin).

odiariaIrA(draco,hufflepuff).


caracteristicasPrincipales(gryffindor,corajudo).

caracteristicasPrincipales(slytherin,orgulloso).
caracteristicasPrincipales(slytherin,inteligente).

caracteristicasPrincipales(ravenclaw,inteligente).
caracteristicasPrincipales(ravenclaw,responsable).

caracteristicasPrincipales(hufflepuff,amistoso).

mago(Mago):-
sangre(Mago,_).

casa(Casa):-
    caracteristicasPrincipales(Casa,_).


% 1 permiteEntrar(Mago,Casa).

permiteEntrar(Mago,Casa):-
    mago(Mago),
    casa(Casa),
    Casa \= slytherin.

permiteEntrar(Mago,slytherin):-
    mago(Mago),
    not(sangre(Mago,impura)).


% 2 tieneCaracterApropiado(Mago,Casa).

tieneCaracterApropiado(Mago,Casa):-
    mago(Mago),
    casa(Casa),
    forall(caracteristicasPrincipales(Casa,Caracteristicas),caracteristicas(Mago,Caracteristicas)).


% 3 enQueCasaQueda(Mago,Casa).

enQueCasaQueda(Mago,Casa):-
    tieneCaracterApropiado(Mago,Casa),
    permiteEntrar(Mago,Casa),
    not(odiariaIrA(Mago,Casa)).

enQueCasaQueda(hermione,gryffindor).

% 4 cadenaDeAmistades(Magos).

cadenaDeAmistades([Mago|[]]):-
    amistoso(Mago).
cadenaDeAmistades([Primero,Segundo |Cola]):-
    amistoso(Primero),
    enQueCasaQueda(Primero,Casa),
    enQueCasaQueda(Segundo,Casa),
    cadenaDeAmistades([Segundo | Cola]).

amistoso(Nombre):-
    caracteristicas(Nombre,amistoso).

% Parte 2

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).


realizoAccion(harry,andarFueraDeLaCama).
realizoAccion(hermione,irAlTercerPiso).
realizoAccion(hermione,irSeccionProhibidaBiblioteca).
realizoAccion(harry,irAlBosque).
realizoAccion(harry,irAlTercerPiso).
realizoAccion(draco,irALasMazmorras).
realizoAccion(ron,ganarPartidaAjedrezMagico).
realizoAccion(hermione,salvarASusAmigos).
realizoAccion(harry,ganarleAVolvemort).
accionRealizada(hermione,respondio(dondeSeEncuentraUnBezoar,20,snape)).
accionRealizada(hermione,respondio(comoHacerLevitarUnaPluma,25,flitwick)).


puntoDeAccion(andarFueraDeLaCama,-50).
puntoDeAccion(irAlBosque,-50).
puntoDeAccion(irSeccionProhibidaBiblioteca,-10).
puntoDeAccion(irAlTercerPiso,-75).
puntoDeAccion(ganarleAVolvemort,60).
puntoDeAccion(ganarPartidaAjedrezMagico,50).
puntoDeAccion(salvarASusAmigos,50).
puntaje(ganarAVoldemort,60).

puntaje(respondio(_,Dificultad,snape),Puntos):-
    Puntos is Dificultad / 2.

puntaje(respondio(_,Dificultad,Profesor),Dificultad):-
    Profesor \= snape.

% 1 a  buenAlumnno(Mago).

buenAlumnno(Mago):-
    realizoAccion(Mago,Accion),
    not(malaAccion(Accion)).

malaAccion(Accion):-
    puntoDeAccion(Accion,Puntaje),
    Puntaje < 0.

% 1 b accionRecurrente(Mago).

accionRecurrente(Accion):-
    realizoAccion(Mago,Accion),
    realizoAccion(OtroMago,Accion),
    Mago \= OtroMago.

% 2 puntajeTotalDeUnaCasa(Casa,PuntajeTotal)

puntajeTotalDeUnaCasa(Casa,PuntajeTotal):-
    casa(Casa),
    findall(Puntos, puntosPorMiembro(Casa,Puntos), ListaDePuntos),
    sum_list(ListaDePuntos, PuntajeTotal).
    
    
puntosPorMiembro(Casa,Puntaje):-
esDe(Mago,Casa),
realizoAccion(Mago,Accion),
puntoDeAccion(Accion,Puntaje).

% 3 casaGanadora(Casa).

casaGanadora(Casa):-
    puntajeTotalDeUnaCasa(Casa,PuntoGanador),
    forall((puntajeTotalDeUnaCasa(OtraCasa,PuntoPerdedor),Casa\=OtraCasa), PuntoGanador > PuntoPerdedor).

