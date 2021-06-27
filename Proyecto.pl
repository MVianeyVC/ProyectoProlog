:- use_module(library(pce)).
:- pce_image_directory('./fotosproyecto').
:- use_module(library(pce_style_item)).
:- dynamic color/2.

resource(p_inicio, image, image('p_inicio.jpg')).

resource(moquillo, image, image('moquillo.jpg')).
resource(rabia, image, image('rabia.jpg')).
resource(parvovirus, image, image('parvovirus.jpg')).
resource(tetanos, image, image('tetanos.jpg')).
resource(tubercolosis, image, image('tubercolosis.jpg')).
resource(leptospirosis, image, image('leptospirosis.jpg')).
resource(complejo_respiratorio_felino, image, image('complejo_r_felino.jpg')).
resource(virus_leucemia_felina, image, image('virus_l_felina.jpg')).
resource(virus_inmunodeficiencia_felina, image, image('virus_i_felina.jpg')).

resource(dificultad_respirar, image, image('dificultad_respirar.jpg')).
resource(secrecion_nasal, image, image('secrecion_nasal.jpg')).
resource(vomito_o_diarrea, image, image('vomito_o_diarrea.jpg')).
resource(nariz_seca, image, image('nariz_seca.jpg')).
resource(cambio_comportamiento, image, image('cambio_comportamiento.jpg')).
resource(sensibilidad_luz, image, image('sensibilidad_luz.jpg')).
resource(salivacion, image, image('salivacion.jpg')).
resource(desorientado, image, image('desorientado.jpg')).
resource(sensible_oidos, image, image('sensible_oidos.jpg')).
resource(diarrea_con_sangre, image, image('diarrea_con_sangre.jpg')).
resource(vomito, image, image('vomito.jpg')).
resource(deshidratado, image, image('deshidratado.jpg')).
resource(letargo, image, image('letargo.jpg')).
resource(perdida_apetito, image, image('perdida_apetito.jpg')).
resource(trismo, image, image('trismo.jpg')).
resource(rigidez, image, image('rigidez.jpg')).
resource(convulsiones, image, image('convulsiones.jpg')).
resource(horner, image, image('horner.jpg')).
resource(retencion, image, image('retencion.jpg')).
resource(astemia, image, image('astenia.jpg')).
resource(anorexia, image, image('anorexia.jpg')).
resource(fiebre, image, image('fiebre.jpg')).
resource(respirar, image, image('respirar.jpg')).
resource(horina_oscura, image, image('horina_oscura.jpg')).
resource(fiebre_alta, image, image('fiebre.jpg')).
resource(deshidratacion, image, image('deshidratado.jpg')).
resource(congestion, image, image('congestion.jpg')).
resource(lagrimeo, image, image('lagrimeo.jpg')).
resource(estornudo, image, image('estornudo.jpg')).
resource(laganas, image, image('laganas.jpg')).
resource(mucosidad, image, image('mucosidad.jpg')).
resource(perdida_peso, image, image('pedida_peso.jpg')).
resource(pelaje, image, image('pelaje.jpg')).
resource(i_piel, image, image('i_piel.jpg')).
resource(pierde_peso, image, image('pedida_peso.jpg')).
resource(encias, image, image('encias.jpg')).
resource(gato_fiebre, image, image('gato_fiebre.jpg')).
resource(actividad, image, image('actividad.jpg')).

nueva_imagen(Ventana, Imagen) :-new(Figura, figure),
new(Bitmap, bitmap(resource(Imagen),@on)),
send(Bitmap, name, 1),
send(Figura, display, Bitmap),
send(Figura, status, 1),
send(Ventana, display,Figura,point(0,0)).

imagen_pregunta(Ventana, Imagen) :-new(Figura, figure),
new(Bitmap, bitmap(resource(Imagen),@on)),
send(Bitmap, name, 1),
send(Figura, display, Bitmap),
send(Figura, status, 1),
send(Ventana, display,Figura,point(500,60)).

botones:-borrado,
send(@boton, free),
mostrar_diagnostico(Enfermedad),
send(@texto, selection('Según los datos el diagnostico es:')),
send(@resp1, selection(Enfermedad)),
new(@boton, button('Comenzar',
message(@prolog, botones))),
send(@main, display,@boton,point(20,450)).

   preguntar(Preg,Resp):-new(Di,dialog('Consultando')),
                        new(L2,label(texto,'Responda lo siguiente.')),
                        id_imagen_preg(Preg,Imagen),
                        imagen_pregunta(Di,Imagen),
                        new(La,label(prob,Preg)),
                        new(B1,button(si,and(message(Di,return,si)))),
                        new(B2,button(no,and(message(Di,return,no)))),
                        send(Di, gap, size(25,25)),
                        send(Di,append(L2)),
                        send(Di,append(La)),
                        send(Di,append(B1)),
                        send(Di,append(B2)),
                        send(Di,default_button,'si'),
                        send(Di,open_centered),get(Di,confirm,Answer),
                        free(Di),
                        Resp=Answer.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
interfaz_principal:-new(@main,dialog('Sistema en enfermedades básicas de mascota',
        size(1000,1000))),
        new(@texto, label(nombre,'Según los datos el diagnostico es:',font('times','roman',18))),
        new(@resp1, label(nombre,'',font('times','roman',24))),
        new(@lblExp1, label(nombre,'',font('times','roman',14))),
        new(@lblExp2, label(nombre,'',font('times','roman',14))),

        new(@salir,button('Salir',and(message(@main,destroy),message(@main,free)))),
        new(@boton, button('Iniciar',message(@prolog, botones))),

        nueva_imagen(@main, p_inicio),
        send(@main, display,@boton,point(138,450)),
        send(@main, display,@texto,point(60,100)),
        send(@main, display,@salir,point(300,450)),
        send(@main, display,@resp1,point(100,150)),


        send(@main,open_centered).

       borrado:- send(@resp1, selection('')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  :-interfaz_principal.

conocimiento('moquillo',
['¿Se presenta dificultad para respirar?', '¿Se presenta secrecion nasal?',
'¿Hay vomito o diarrea?','¿Se presenta nariz seca?']).

conocimiento('rabia',
['¿Presenta cambios de comportamiento?', '¿Se muestra sensible a la luz?',
'¿Tiene salivacion exesiva?','¿Hay desorientacion?','¿Hay sensibilidad en oidos?']).

conocimiento('parvovirus',
['¿Presenta diarrea con sangre?','¿Presenta vomitos?','¿Se nota deshidratado?','¿Tiene letargo?','¿Hay perdida de apetito?']).

conocimiento('tetanos',
['¿Presenta trismo?','¿Presenta rigidez en algun miembro del cuerpo?',
 '¿Tiene convulsiones?','¿Presencia de horner?','¿Hay retención de horina?']).

conocimiento('tubercolosis',
['¿Presenta astemia?','¿Presenta anorexia?',
 '¿Tiene fiebre?','¿Dificultad para respirar?']).

conocimiento('leptospirosis',
['¿Orina color oscura?','¿Letargo?','¿Fiebre alta?','¿Deshidratacion?','¿Congestion nasal?']).

conocimiento('complejo_respiratorio_felino',
['¿Hay lagrimeo?','¿Tiene estornudos?','¿Laganas en ojos?','¿Mucosidad nasal?']).

conocimiento('virus_leucemia_felina',
['¿Hay perdida de peso o apetito?','¿Deterioro de pelaje?','¿Infecciones en la piel?']).

conocimiento('virus_inmunodeficiencia_felina',
['¿Hay perdida de peso?','¿Deterioro de encias?','¿Fiebre?','¿Bajo nivel de actividad?']).


id_imagen_preg('¿Se presenta dificultad para respirar?','dificultad_respirar').
id_imagen_preg('¿Se presenta secrecion nasal?','secrecion_nasal').
id_imagen_preg('¿Hay vomito o diarrea?','vomito_o_diarrea').
id_imagen_preg('¿Se presenta nariz seca?','nariz_seca').

id_imagen_preg('¿Presenta cambios de comportamiento?','cambio_comportamiento').
id_imagen_preg('¿Se muestra sensible a la luz?','sensibilidad_luz').
id_imagen_preg('¿Tiene salivacion exesiva?','salivacion').
id_imagen_preg('¿Hay desorientacion?','desorientado').
id_imagen_preg('¿Hay sensibilidad en oidos?','sensible_oidos').

id_imagen_preg('¿Presenta diarrea con sangre?','diarrea_con_sangre').
id_imagen_preg('¿Presenta vomitos?','vomito').
id_imagen_preg('¿Se nota deshidratado?','deshidratado').
id_imagen_preg('¿Tiene letargo?','letargo').
id_imagen_preg('¿Hay perdida de apetito?','perdida_apetito').


id_imagen_preg('¿Presenta trismo?','trismo').
id_imagen_preg('¿Presenta rigidez en algun miembro del cuerpo?','rigidez').
id_imagen_preg('¿Tiene convulsiones?','convulsiones').
id_imagen_preg('¿Presencia de horner?','horner').
id_imagen_preg('¿Tiene convulsiones?','convulsiones').
id_imagen_preg('¿Hay retención de horina?','retencion').

id_imagen_preg('¿Presenta astemia?','astemia').
id_imagen_preg('¿Presenta anorexia?','anorexia').
id_imagen_preg('¿Tiene fiebre?','fiebre').
id_imagen_preg('¿Dificultad para respirar?','respirar').

id_imagen_preg('¿Orina color oscura?','horina_oscura').
id_imagen_preg('¿Letargo?','letargo').
id_imagen_preg('¿Fiebre alta?','fiebre_alta').
id_imagen_preg('¿Deshidratacion?','deshidratacion').
id_imagen_preg('¿Congestion nasal?','congestion').

id_imagen_preg('¿Hay lagrimeo?','lagrimeo').
id_imagen_preg('¿Tiene estornudos?','estornudo').
id_imagen_preg('¿Laganas en ojos?','laganas').
id_imagen_preg('¿Mucosidad nasal?','mucosidad').

id_imagen_preg('¿Hay perdida de peso o apetito?','perdida_peso').
id_imagen_preg('¿Deterioro de pelaje?','pelaje').
id_imagen_preg('¿Infecciones en la piel?','i_piel').

id_imagen_preg('¿Hay perdida de peso?','pierde_peso').
id_imagen_preg('¿Deterioro de encias?','encias').
id_imagen_preg('¿Fiebre?','gato_fiebre').
id_imagen_preg('¿Bajo nivel de actividad?','actividad').


:- dynamic conocido/1.

  mostrar_diagnostico(X):-haz_diagnostico(X),limpiar_ventana.
  mostrar_diagnostico(diagnostico_desconocido):-limpiar_ventana.

  haz_diagnostico(Diagnosis):-
                            obten_hipotesis_y_sintomas(Diagnosis,
                            ListaDeSintomas),
                            prueba_presencia_de(Diagnosis,
                            ListaDeSintomas).


obten_hipotesis_y_sintomas(Diagnosis, ListaDeSintomas):-
                           conocimiento(Diagnosis, ListaDeSintomas).


prueba_presencia_de(Diagnosis, []).
prueba_presencia_de(Diagnosis, [Cabeza | Cola]):-
  prueba_verdad_de(Diagnosis, Cabeza), prueba_presencia_de(Diagnosis, Cola).


prueba_verdad_de(Diagnosis, Sintoma):- conocido(Sintoma).
prueba_verdad_de(Diagnosis, Sintoma):-
 not(conocido(is_false(Sintoma))),
 pregunta_sobre(Diagnosis, Sintoma, Resp), Resp = 'si'.


 pregunta_sobre(Diagnosis, Sintoma, Resp):-
 preguntar(Sintoma,Respuesta),
   process(Diagnosis, Sintoma, Respuesta, Resp).


process(Diagnosis, Sintoma, si, si):- asserta(conocido(Sintoma)).

process(Diagnosis, Sintoma, no, no):- asserta(conocido(is_false(Sintoma))).


limpiar_ventana:- retract(conocido(X)), fail.
limpiar_ventana.


conocido(_):- fail.

not(X):- X,!,fail.
not(_).


















