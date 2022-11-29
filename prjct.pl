
%  Rafael Alves ist199308

%--------------------------------------------------------------

% maior_zero(X)
% Testa se X e maior que 0.
maior_zero(X):-
    X>0.

% obter_indice(InAtual,InNovo)
% Incrementa em 1 uma variavel.
obter_indice(InAtual,InNovo):-
    InNovo is InAtual+1.

% decr_ind(IndAtual,IndDec)
% Decrementa em 1 uma variavel.
decr_ind(IndAtual,IndDec):-
    IndDec is IndAtual -1.


%------------------------------------------------------------------

% extrai_ilhas_linha(N_L, Linha, Ilhas) /3
% Ilhas eh a lista ordenada de ilhas da linha Linha que corresponde a N_L linha do puzzle.

%Predicados Auxiliares:

% extrai_ilhas_linha_indice( Indice,[P|R], ListaIndices) 
% Predicado auxiliar que encontra o indice das ilhas de uma lista.

% extrai_ilhas_linha_res(N_L ,[P|R],[T|V] ,Ilhas)
% Predicado que com uma lista dos indices das ilhas de uma lista e a lista
% com o valor (numero de pontes da ilha) calcula a lista ordenada de ilhas.

extrai_ilhas_linha_indice( _,[] , []).

extrai_ilhas_linha_indice( Indice,[P|R], ListaIndices):-
    P =:=0 ,
    obter_indice(Indice,InNovo),
    extrai_ilhas_linha_indice(InNovo, R  , ListaIndices) ;

    P > 0 ,
    obter_indice(Indice,InNovo),
    ListaIndices = [InNovo| ListaIndicesAux],
    extrai_ilhas_linha_indice(InNovo, R  ,ListaIndicesAux) .
    

extrai_ilhas_linha_res(N_L ,[P|R],[T|V] ,Ilhas):-
    Ilhas = [ ilha(P,(N_L,T))| IlhasAux],
    extrai_ilhas_linha_res(N_L, R,V,  IlhasAux).

extrai_ilhas_linha_res(_,[],[] ,[]).

extrai_ilhas_linha(N_L, Linha, Ilhas):-
    extrai_ilhas_linha_indice(0,Linha , ListaIndice),
    include(maior_zero , Linha , ListaValor),
    extrai_ilhas_linha_res(N_L,ListaValor ,ListaIndice ,Ilhas).

extrai_ilhas_linha( _ , [] , [] ).
%-------------------------------------------------------------------

%-------------------------------------------------------------------
% ilhas(Puzzle, Ilhas)  /2
% Ilhas eh a lista de ilhas (da esquerda para a direita e de cima para baixo) de puzzle. 

ilhas([], []).

ilhas(Puzzle, Ilhas):-
    length(Puzzle, L),
    reverse(Puzzle, [P |R]),
    extrai_ilhas_linha(L, P, Ilhas_Ext),
    append(Ilhas_Aux, Ilhas_Ext, Ilhas),
    reverse(R, R1),
    ilhas(R1, Ilhas_Aux).

%--------------------------------------------------------------------

%---------------------------------------------------------------------
% vizinhas(Ilhas, Ilha, Vizinhas) /3
% Vizinhas eh a lista ordenada de ilhas (da esquerda para a direita e de cima para baixo ) 
% vizinhas de Ilha.  Ilhas eh a lista de ilhas de um puzzle.

% Predicados auxiliares:

% verifica_ilhas(Ilhas, ilha(_,(L1,C1))  ,ilha(_,(L2,C2))  /3
% Predicado que com a lista de ilhas de um puzzle e duas ilhas verifica se entre 
% elas nao existe uma ponte que una outras duas ilhas. 

% check_mesma_lc(ilha(_,(L2,C2)),(L1,C1)) /2
% Predicado que com uma ilha e uma posicao verifica se a ilha tem a mesma linha ou coluna
% da posicao.

% obter_pos(ilha(_,(L1,C1)),(L1,C1))  /2
% Predicado que recebe uma ilha e calcula a sua posicao no puzzle.


obter_pos(ilha(_,(L1,C1)),(L1,C1)).

check_mesma_lc(ilha(_,(L2,C2)),(L1,C1)):-
    posicoes_entre((L2,C2),(L1,C1),_),
    L1 =\= L2 ;
    posicoes_entre((L2,C2),(L1,C1),_),
    C1 =\= C2.
    

verifica_ilhas(Ilhas, ilha(_,(L1,C1))  ,ilha(_,(L2,C2))  ):-
    posicoes_entre((L2,C2),(L1,C1),ListaEntre),
    maplist(obter_pos,Ilhas,Posicoes),
    intersection(ListaEntre , Posicoes ,Res),
    Res = [].

vizinhas(Ilhas, ilha(_,(L1,C1)), Vizinhas):-
    findall(I,(member(I,Ilhas), check_mesma_lc( I,(L1,C1))),VizinhasAux),
    findall(V,(member(V,VizinhasAux),verifica_ilhas(Ilhas ,V,ilha(_,(L1,C1)))),Vizinhas). 

% -------------------------------------------------------------------------------

% -------------------------------------------------------------------------------
% estado(Ilhas, Estado)  /2
% Estado eh a lista de entradas de cada ilha de Ilhas. A lista de pontes de cada entrada e vazia
% na criacao de um estado.
% Entrada eh uma lista que contem uma ilha , a lista das vizinhas da ilha e a lista 
%de pontes da ilha

% Predicados Auxiliares:

% estado_entrada(Ilhas,Ilha,Res) / 3
% Predicado auxiliar que recebe uma lista de ilhas(Ilhas) e uma ilha e calcula a entrada de Ilha.


estado_entrada(Ilhas,Ilha,Res):-
    vizinhas(Ilhas,Ilha,IlhasVizinhas),
    Res=[Ilha , IlhasVizinhas ,[]].

estado(Ilhas, Estado):-
    maplist( estado_entrada(Ilhas) ,Ilhas,Estado).

% -----------------------------------------------------------------------------------

% -----------------------------------------------------------------------------------
% posicoes_entre(Pos1, Pos2, Posicoes) /3
% Posicoes e a lista ordenada de ilhas entre as posicoes Pos1 e Pos2. Pos1 e Pos2 nao pertencem a
% Posicoes. Caso as posicoes nao pertenca a mesma linha ou coluna o resultado é false.

% Predicados Auxiliares:

% check_posicoes((L1,C1),(L2,C2))   /2
% Verifica se duas posicoes sao diferentes.

% tirar_primeiro([_|R],R) /2
% Predicado que retira o primeiro elemento de uma lista.

% calcular_posicoes_auxL(L, C1, C2, Posicoes) e calcular_posicoes_auxC(C, L1, L2, Posicoes) /4 e /4
% Predicados que recebem uma linha e duas colunas (ou uma coluna e duas linhas) e calcula
% as posicoes entre. 

% calcular_posicoes((L1,C1),(L2,C2),Posicoes) /3
% Predicado que com duas posicoes calcula as posicoes entre elas.

check_posicoes((L1,C1),(L2,C2)):-
    L1 =\= L2 ; 
    C1 =\= C2. 

tirar_primeiro([],[]).
tirar_primeiro([_|R],R).


calcular_posicoes_auxL(L, C1, C2, Posicoes):-
    findall((L,X),between(C1,C2,X),V),
    tirar_primeiro(V,Res),
    reverse(Res,Res2),
    tirar_primeiro(Res2,Res3),
    reverse(Res3,Posicoes).

calcular_posicoes_auxC(C, L1, L2, Posicoes):-
    findall((X,C),between(L1,L2,X),V),
    tirar_primeiro(V,Res),
    reverse(Res,Res2),
    tirar_primeiro(Res2,Res3),
    reverse(Res3,Posicoes).

calcular_posicoes((L1,C1),(L2,C2),Posicoes):-
    L1 =:= L2, C1 > C2,
    calcular_posicoes_auxL(L1, C2, C1, Posicoes);
    L1 =:= L2, C1 < C2,
    calcular_posicoes_auxL(L1, C1, C2, Posicoes);
    C1 =:= C2, L1 > L2,
    calcular_posicoes_auxC(C1, L2, L1, Posicoes);
    C1 =:= C2, L1 < L2,
    calcular_posicoes_auxC(C1, L1, L2, Posicoes).

    
posicoes_entre((L1,C1),(L2,C2), Posicoes):-
    check_posicoes((L1,C1),(L2,C2)),
    calcular_posicoes((L1,C1),(L2,C2),Posicoes).
   
% -------------------------------------------------------------------------

% --------------------------------------------------------------------------
% cria_ponte(Pos1, Pos2, Ponte)    /3
% Ponte eh a ponte entre entre Pos1 e Pos2.

% Predicados Auxiliares:

% check_posicao_menor((L1,C1),(L2,C2),(L3,C3),(L4,C4)) /4
% Predicado que com duas posicoes verifica qual delas e a posicao menor.Posicoes aumentam 
% da esquerda para a direita e de cima para baixo logo (1,1) sera menor que (5,1) ou que (1,5). 
check_posicao_menor((L1,C1),(L2,C2),(L3,C3),(L4,C4)):-
    (L1 > L2 ,
    C1 =:= C2 ;
    C1 > C2,
    L1 =:= L2 ),
    L3 = L2 , C3 = C2 , L4 = L1 ,C4 = C1;

    L3 = L1 , C3 = C1 , L4 = L2 ,C4 = C2.

cria_ponte((L1,C1),(L2,C2), Ponte):-
    check_posicao_menor((L1,C1),(L2,C2),(L3,C3),(L4,C4)),
    Ponte = ponte((L3,C3),(L4,C4)).

% -----------------------------------------------------------------

% -----------------------------------------------------------------
% caminho_livre((L1,C1), (L2,C2), Posicoes,Ilha, Vizinha) /5
% Posicoes e a lista de Posicoes entre Pos1-(L1,C1) e Pos2-(L2,C2). Adicionar uma ponte
% entre estas duas posicoes nao faz com que Ilha e Vizinha(uma ilha vizinha de Ilha) deixem de ser
% vizinhas.

caminho_livre((L1,C1), (L2,C2), Posicoes,ilha(_,(L3,C3)), ilha(_,(L4,C4))):-
    L1 = L3 ,C1 = C3 ,L2 = L4 , C2 = C4 ; L1 = L4 ,C1 = C4 , L2 = L3 ,C2=C3; 
    posicoes_entre((L3,C3), (L4,C4),Pos_entre_IVZ),
    intersection(Pos_entre_IVZ , Posicoes,Res),
    Res=[].

% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
% actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada,Nova_Entrada) /5
% Nova_Entrada eh a Entrada atualizada caso se adicionasse uma ponte entre Pos1 e Pos2.
% Posicoes e a lista de posicoes entre Pos1 e Pos2.

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada,Nova_Entrada):-
    Entrada = [Ilha | [Vizinhas | Pontes ]],
    findall(V, (member(V, Vizinhas), caminho_livre(Pos1, Pos2, Posicoes, Ilha, V)),Nova_Vizinhas),
    Nova_Entrada = [Ilha | [Nova_Vizinhas | Pontes]].

% -----------------------------------------------------------------------------------

% ----------------------------------------------------------------------------------
% actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) /4
% Atualiza Estado depois de adicionar uma ponte entre Pos1 e Pos2.

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado):-
    posicoes_entre(Pos1, Pos2, Posicoes),
    maplist(actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes),Estado,Novo_estado).

% ------------------------------------------------------------------------------------

% --------------------------------------------------------------------------
% ilhas_terminadas(Estado , Ilhas_term) /2
% Ilhas_term eh a lista das ilhas que ja tem todas as pontes associadas , ou seja , as ilhas em que
% N_Pontes eh diferente de 'X' e o comprimento da lista Pontes for igual a N_Pontes.

% Predicados Auxiliares:

% check_length(N_P , Pontes) /2
% Verifica se a length de Pontes eh igual a N_P.

check_length(N_P , Pontes):-
    length(Pontes,LP),
    N_P = LP .

ilhas_terminadas(Estado , Ilhas_term):-
    findall([ilha(N_P,(L1,C1)),_,Pontes] ,(member( [ilha(N_P,(L1,C1)),_,Pontes],Estado),not(N_P='X')),Ilhas_termAux),
    findall( ilha(N_P,(L1,C1))  , (member([ilha(N_P,(L1,C1)),_,Pontes],Ilhas_termAux) , check_length(N_P,Pontes) ),Ilhas_term).

% ---------------------------------------------------------------------------------------

%-----------------------------------------------------------------------------------------
% tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada) /3
% Nova_entrada eh a Entrada atualizada apos de remover as ilhas terminadas 
% da lista de vizinhas da entrada.

tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada):-
    Entrada = [Ilha | [Vizinhas | Pontes ]],
    findall(V,(member(V, Vizinhas), not(member(V,Ilhas_term))),Nova_Vizinhas),
    Nova_entrada = [Ilha | [Nova_Vizinhas | Pontes]].

% ----------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------
% tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) /3
% Novo_estado eh o Estado atualizado apos tirarmos as ilhas terminadas a cada entrada do Estado.

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):-
     maplist( tira_ilhas_terminadas_entrada(Ilhas_term),Estado,Novo_estado ).
% -------------------------------------------------------------------------------

% -----------------------------------------------------------------------------------
% marca_ilhas_terminadas_entrada(Ilhas_term, Entrada,Nova_entrada) /3
% Entrada eh atualizada caso a sua ilha pertencer a Ilhas_term.

marca_ilhas_terminadas_entrada(Ilhas_term, Entrada,Nova_entrada):-
    Entrada=  [Ilha | _],
    not(member(Ilha,Ilhas_term)),
    Nova_entrada = Entrada

    ;
    
    Entrada=  [Ilha | R],
    Ilha = ilha(_,Pos),
    member(Ilha,Ilhas_term),
    IlhaRes = ilha('X',Pos),
    Nova_entrada = [IlhaRes | R] .
% ------------------------------------------------------------------------------------

% -------------------------------------------------------------------------------------

% marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) /3
% Novo_estado eh o Estado atualizado apos marcamos as ilhas terminadas a cada entrada do Estado.

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term),Estado,Novo_estado).

% ------------------------------------------------------------------------------------

% ------------------------------------------------------------------------------------ 
% trata_ilhas_terminadas(Estado, Novo_estado)
% Novo_estado eh o Estado atualizado apos tirarmos as ilhas terminadas e marcamos as ilhas terminadas
% a cada entrada do Estado.

trata_ilhas_terminadas(Estado, Novo_estado):-
    ilhas_terminadas(Estado,Ilha_T),
    tira_ilhas_terminadas(Estado,Ilha_T ,Novo_estadoAux),
    marca_ilhas_terminadas(Novo_estadoAux,Ilha_T ,Novo_estado).

% --------------------------------------------------------------------------------------

pertence(_,[]).
pertence(E, [P|R]):-
    E==P;
    pertence(E,R).


nao_pertence(_,[]).

nao_pertence(E, [P|R]):-
    E\==P,
    nao_pertence(E,R).

mult_N([],_,[]).
mult_N([P | R],N,[P_N | R_N]) :-
    P_N is P * N,
    mult_N(R,N,R_N).

el_maior([P|_]):-
    P > 5 .
    
el_maior([_|R]):- el_maior(R).


insere_ordenado(El,[P|R],L2):-
    El < P , 
    L2 =[ El | [P|R] ] ; 
    El > P ,
    insere_ordenado(El,R , L3 ),
    L2 = [P | L3] .

insere_ordenado(X, [], [X]).

d([P|R],[P,P|Aux]):-
    d(R,Aux).

d([],[]).

repete(El,1,[El]).
repete(El,N,[El|R]):-
    N > 1 ,
    N1 is N -1 ,
    repete(El,N1,R).

soma(N,S):-
    A is mod(N,10),
    N2 is N // 10,
    S2 is S + A,
    soma(N2,S2).

soma(0,S).