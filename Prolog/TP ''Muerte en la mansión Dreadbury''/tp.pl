%% TP Logico: Muerte en la mansion Dreadbury

%% Pablo Daniel Riquelme Blaffet - Lunes Mañana

%% Base de Conocimientos - Quien vive en la mansion

viveEnMansion(tiaAgatha).
viveEnMansion(mayordomo).
viveEnMansion(charles).

%% Quien odia a quien

odia(tiaAgatha, Persona) :-
    viveEnMansion(Persona),
    Persona \= mayordomo.

odia(mayordomo, Persona) :-
    viveEnMansion(Persona),
    odia(tiaAgatha, Persona).

odia(charles, Persona) :-
    viveEnMansion(Persona),
    not(odia(tiaAgatha, Persona)).

%% Quien es mas rico

esMasRico(Persona, tiaAgatha) :-
    viveEnMansion(Persona),
    not(odia(mayordomo, Persona)).

%% Quien mato a quien

quienMato(Persona, PersonaAsesinada) :-
    viveEnMansion(Persona),
    odia(Persona, PersonaAsesinada),
    not(esMasRico(Persona, PersonaAsesinada)).

/*

Punto 1

    b) Para saber quien mato a la Tia Agatha podemos usar una consulta de tipo existencial.

        ?- quienMato(Persona, tiaAgatha).
        Persona = tiaAgatha.

Punto 2

    a)

    i) Para saber si alguien odia a milhouse se debe hacer una consulta de tipo existencial y nos devuelve:
        ?- odia(_, milhouse).
        false.

    ii) Para saber que personas odia Charles, tambien se hace una consulta de tipo existencial.

        ?- odia(charles, Persona).
        Persona = mayordomo.

    iii) Para saber que personas odian a la tia Agatha, hacemos una consulta existencial de la forma:

        ?- odia(Persona, tiaAgatha).
        Persona = tiaAgatha,
        Persona = mayordomo.

    iv) Se realiza una consulta de tipo existencial

        -? odia(Persona, OtraPersona).
        Persona = OtraPersona, OtraPersona = tiaAgatha ;
        Persona = tiaAgatha,
        OtraPersona = charles ;
        Persona = mayordomo,
        OtraPersona = tiaAgatha ;
        Persona = mayordomo,
        OtraPersona = charles ;
        Persona = charles,
        OtraPersona = mayordomo ;

    v) Para saber si es cierto que el mayordomo odia a alguien se debe hacer una consulta existencial de la forma:

        -? odia(mayordomo, _).
        true.

*/