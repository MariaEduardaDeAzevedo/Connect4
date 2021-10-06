:- dynamic topo/2.
:- dynamic tamanho/2.
:- dynamic peca/3.
:- dynamic vence/1.
:- dynamic perde/1.
:- dynamic eval/3.
:- dynamic pontuacao/2.
:- dynamic profundidade/1.
:- dynamic estrategia/1.
:- dynamic vez/1.

% Para iniciar o jogo:
% swipl main.pl
% ?- connect4()

pontuacao(vermelho,0).
pontuacao(amarelo,0).

%%%%%%%%%%%  Iniciar Tabuleiro %%%%%%%%%%%

iniciar_topo(0) :- !.
iniciar_topo(C) :- assert(topo(C,0)),NovaColuna is C-1, iniciar_topo(NovaColuna) .

iniciar(C,R) :-
	retractall(tamanho(_,_)),
	retractall(profundidade(_)),
	retractall(vez(_)),
	retractall(pontuacao(_,_)),
	retractall(estrategia(_)),
	assert(pontuacao(vermelho,0)),
	assert(pontuacao(amarelo,0)),
	assert(tamanho(C,R)),
	retractall(topo(_,_)),
	iniciar_topo(C),
	retractall(peca(_,_,_)),
	reiniciar().

%%%%%%%%%%% Validacao da Vitoria %%%%%%%%%%%

reiniciar() :-
	retractall(vence(_)),
	retractall(perde(_)).

checarStatus(X,Y,Jogador) :-
	checar(X,Y),
	( Jogador == vermelho -> assert(vence(vermelho)),assert(perde(amarelo));
	  Jogador == amarelo -> assert(vence(amarelo)),assert(perde(vermelho))
	).
checarStatus(_,_,_).

desenharTabuleiro() :-
	tamanho(C,R),
	aggregate_all(count,topo(_,R),Cnt),
	Cnt == C.

traverse(C,R,IncrementaColuna,IncrementaLinha,Res) :-
       NovaColuna is C + IncrementaColuna,
       NovaLinha is R + IncrementaLinha,
       peca(C,R,C1),
       peca(NovaColuna,NovaLinha,C2),
       C1 == C2,
       traverse(NovaColuna,NovaLinha,IncrementaColuna,IncrementaLinha,Res1),
       Res is Res1 + 1,!.
traverse(_,_,_,_,Res) :-
	Res is 1.

% Checagem horizontal
checar(X,Y) :-
	traverse(X,Y,1,0,R1),
	traverse(X,Y,-1,0,R2),
	R is R1 + R2 - 1  ,
	R >= 4,!.

% Checagem vertical
checar(X,Y) :-
	traverse(X,Y,0,1,R1),
	traverse(X,Y,0,-1,R2),
	R is R1 + R2 - 1,
	R >= 4,!.

% Checagem da diagonal principal
checar(X,Y) :-
	traverse(X,Y,1,1,R1),
	traverse(X,Y,-1,-1,R2),
	R is R1 + R2 - 1,
	R >= 4,!.

% Checagem da diagonal secundária
checar(X,Y) :-
	traverse(X,Y,1,-1,R1),
	traverse(X,Y,-1,1,R2),
	R is R1 + R2 - 1,
	R >= 4,!.


%%%%%%%%%%%  Regras de Entrada e Saida %%%%%%%%%%%

desenharPeca(vermelho) :- write('\033[1;31m\033[1;44mo\033[0;0m').
desenharPeca(amarelo) :- write('\033[1;33m\033[1;44mo\033[0;0m').

print() :-
	tamanho(_,Y),
	print(1,Y).

print(C,R) :-
	tamanho(X,_),
	C == X,
	R == 1,
	peca(C,R,Jogador),
	desenharPeca(Jogador),
	write('\033[1;44m  \033[0;0m'),!.
print(C,R) :-
	tamanho(X,_),
	C == X,
	R == 1,
	not(peca(C,R,_)),
	write('\033[1;97m\033[1;44mo  \033[0;0m'),!.

print(C,R) :-
	tamanho(X,_),
	NovaColuna is C+1,
	peca(C,R,Jogador),
	desenharPeca(Jogador),
	write('\033[1;44m  \033[0;0m'),
	C < X ,
	print(NovaColuna,R),!.
print(C,R) :-
	tamanho(X,_),
	NovaColuna is C+1 ,
	not(peca(C,R,_)) ,
	write('\033[1;97m\033[1;44mo  \033[0;0m') ,
	C < X   ,
	print(NovaColuna,R),!.

print(_,R) :-
	NovaLinha is R-1 ,
	writeln(''),
	R >= 0,
	print(1,NovaLinha).

getEntrada(Jogador) :-
	write('\nIndique uma coluna\n[1.] [2.] [3.] [4.] [5.] [6.]\n'),
	read(C),
	inserir(C,Jogador),
	topo(C,R),
	print(),
	(checarStatus(C,R,Jogador),writeln('');!),!.
getEntrada(Jogador) :-
	writeln('Jogada inválida!'),getEntrada(Jogador).

%%%%%%%%%%%  Ferramentas  %%%%%%%%%%%

inserir(C,Jogador) :-
	tamanho(_,R),
	topo(C,H),
	H < R,
	TmpH is H + 1,
  retractall(topo(C,_)),
  assert(topo(C,TmpH)),
  assert(peca(C,TmpH,Jogador)),
	pontuacao(Jogador,V),
	eval(C,TmpH,E),
	V1 is V + E,
	retractall(pontuacao(Jogador,_)),
	assert(pontuacao(Jogador,V1));
	true.


remover(C):-
	topo(C,H),
	peca(C,H,Jogador),
	retractall(peca(C,H,_)),
	TmpH is H - 1,
  retractall(topo(C,_)),
  assert(topo(C,TmpH)),
	pontuacao(Jogador,V),
	eval(C,H,E),
	V1 is V - E,
	retractall(pontuacao(Jogador,_)),
	assert(pontuacao(Jogador,V1));
	true.

%%%%%%%%%%%%  Regras de Inicio do Jogo %%%%%%%%%%%%%%

connect4():-
	iniciar(6,7),
	avaliarTabuleiro(),
	writeln('Selecione o modo de jogo:\n[1.] Jogador vs Computador\n[2.] Jogador vs Jogador\n:'),
	read(S),
	assert(estrategia(S)),
	( S == 1 ->
	    writeln('Selecione o nível de dificuldade:\n[1.] [2.] [3.]'),
	    read(D),
	    assert(profundidade(D)); true
	),
	writeln('\nVez do \033[1;31mVERMELHO\033[0;0m:'),
	jogar(), halt.

jogar():-
	desenharTabuleiro().
jogar():-
	estrategia(S),
	( S == 1 -> (getEntrada(vermelho),
		  vence(vermelho),
			writeln('\033[1;31mVERMELHO\033[0;0m ganhou!'),halt;
			writeln('\nVez do \033[1;33mAMARELO\033[0;0m:')),
	    ( profundidade(D),
	      getMelhorJogada(amarelo,D,_,Move),
		    inserir(Move,amarelo),
		    print(),
	      topo(Move,H),
	      ( checar(Move,H),writeln('\n\033[1;33mAMARELO\033[0;0m ganhou!'),halt;
				  writeln('\n\nVez do \033[1;31mVERMELHO\033[0;0m:'),not(checar(Move,H))
		    )
       );
	    S == 2 ->(getEntrada(vermelho),
				vence(vermelho),
				writeln('\033[1;31mVERMELHO\033[0;0m ganhou!'),halt;
				writeln('\nVez do \033[1;33mAMARELO\033[0;0m:')),
				( getEntrada(amarelo),
				  vence(amarelo),
				  writeln('\033[1;33mAMARELO\033[0;0m ganhou!'),halt;
				  writeln('\nVez do \033[1;31mVERMELHO\033[0;0m:')
        )
	), jogar().


getResultado([H,T|_],H,T).
getMelhorJogada(Jogador, Profundidade, MelhorPontuacao, BestMove):-
	tamanho(C,_),
	setof(X,between(1,C,X),MoveList),
	minimax(MoveList,Profundidade, Jogador, Result),
	sort(Result,ResultadoOrdenado),
	(profundidade(D),Profundidade == D,writeln('');true),
	( Jogador == vermelho -> nth0(0,ResultadoOrdenado,Move), getResultado(Move,MelhorPontuacao,BestMove);
	  Jogador == amarelo -> length(ResultadoOrdenado,Len), nth1(Len,ResultadoOrdenado,Move), getResultado(Move,MelhorPontuacao,BestMove)
	).


%%%%%%%%%%%% Minimax %%%%%%%%%%%

minimax([],_,_,[]).
minimax([H|T],Profundidade, Jogador, L) :-
	Jogador == vermelho,
	minimax(T,Profundidade,Jogador,L1),
	topo(H,Altura),
	tamanho(_,R),
	( Altura == R -> L = L1,!;
	  inserir(H,Jogador),
	  topo(H,Hight),
	  checarStatus(H,Hight,Jogador),
	  ( vence(vermelho) -> V0 is -1000000000  * (Profundidade + 1), append([[V0,H]],L1,L);
		  vence(amarelo) -> V0 is 1000000000   * (Profundidade + 1), append([[V0,H]],L1,L);
      desenharTabuleiro() -> append([[0,H]],L1,L);
	    not(vence(vermelho);vence(amarelo);desenharTabuleiro()) ->
	      ( Profundidade == 0 -> pontuacao(vermelho,Vv),pontuacao(amarelo,Va), V0 is Va - Vv, append([[V0,H]],L1,L);
		      Profundidade > 0 -> NovaProfundidade is Profundidade - 1, getMelhorJogada(amarelo, NovaProfundidade, MelhorPontuacao,_), append([[MelhorPontuacao,H]],L1,L)
		    )
	  ),
	  reiniciar(),
	  remover(H)
	).


minimax([H|T],Profundidade, Jogador, L) :-
	Jogador == amarelo,
	minimax(T,Profundidade,Jogador,L1),
	topo(H,Altura),
	tamanho(_,R),
	( Altura == R -> L = L1, !;
    inserir(H,Jogador),
	  topo(H,Hight),
	  checarStatus(H,Hight,Jogador),
	  ( vence(vermelho) -> V0 is -1000000000 * (Profundidade + 1), append([[V0,H]],L1,L);
		  vence(amarelo) -> V0 is 1000000000 * (Profundidade + 1), append([[V0,H]],L1,L);
		  desenharTabuleiro() -> append([[0,H]],L1,L);
      not(vence(vermelha);vence(amarelo);desenharTabuleiro()) ->
	      ( Profundidade == 0 -> pontuacao(vermelho,Vv),pontuacao(amarelo,Va), V0 is Va - Vv, append([[V0,H]],L1,L);
    	    Profundidade > 0 -> NovaProfundidade is Profundidade - 1, getMelhorJogada(vermelho, NovaProfundidade, MelhorPontuacao,_), append([[MelhorPontuacao,H]],L1,L)
		    )
	  ),
	  reiniciar(),
	  remover(H)
	).

%%%%%%%%%%%%  Avaliacao de Regras %%%%%%%%%%%%

avaliarTabuleiro():-
	tamanho(_,R),
	retractall(eval(_,_,_)),
	avaliarLinha(R),
	calcularLinha(R).

avaliarColuna(_,0) :- !.
avaliarColuna(LinhaAtual,ColunaAtual) :-
	NovaColuna is ColunaAtual - 1,
	avaliarColuna(LinhaAtual,NovaColuna),
	assert(eval(LinhaAtual,ColunaAtual,0)).

avaliarLinha(0) :- !.
avaliarLinha(LinhaAtual) :-
	NovaLinha is LinhaAtual - 1,
	avaliarLinha(NovaLinha),
	tamanho(C,_),
	avaliarColuna(LinhaAtual,C).

calcularColuna(_,0) :- !.
calcularColuna(LinhaAtual,ColunaAtual) :-
	NovaColuna is ColunaAtual - 1,
	calcularColuna(LinhaAtual,NovaColuna),
	calcularHorizontal(LinhaAtual,ColunaAtual),
	calcularVertical(LinhaAtual,ColunaAtual),
	calcularDiagonalPrincipal(LinhaAtual,ColunaAtual),
	calcularDiagonalSecundaria(LinhaAtual,ColunaAtual).

calcularLinha(0):-!.
calcularLinha(LinhaAtual):-
	NovaLinha is LinhaAtual - 1,
	calcularLinha(NovaLinha),
	tamanho(C,_),
	calcularColuna(LinhaAtual,C).

calcularHorizontal(X,Y) :-
	( Y1 is Y+1, Y2 is Y+2, Y3 is Y+3,
    eval(X,Y,E),eval(X,Y1,E1),eval(X,Y2,E2),eval(X,Y3,E3),
    Ne  is E+1 ,retractall(eval(X,Y,E))  ,assert(eval(X,Y,Ne)),
    Ne1 is E1+1,retractall(eval(X,Y1,E1)),assert(eval(X,Y1,Ne1)),
    Ne2 is E2+1,retractall(eval(X,Y2,E2)),assert(eval(X,Y2,Ne2)),
    Ne3 is E3+1,retractall(eval(X,Y3,E3)),assert(eval(X,Y3,Ne3))
  ); true.

calcularVertical(X,Y) :-
	( X1 is X+1, X2 is X+2, X3 is X+3,
    eval(X,Y,E),eval(X1,Y,E1),eval(X2,Y,E2),eval(X3,Y,E3),
    Ne  is E+1 ,retractall(eval(X,Y,E))  ,assert(eval(X,Y,Ne)),
    Ne1 is E1+1,retractall(eval(X1,Y,E1)),assert(eval(X1,Y,Ne1)),
    Ne2 is E2+1,retractall(eval(X2,Y,E2)),assert(eval(X2,Y,Ne2)),
    Ne3 is E3+1,retractall(eval(X3,Y,E3)),assert(eval(X3,Y,Ne3))
  ); true.

calcularDiagonalPrincipal(X,Y) :-
	( X1 is X+1, X2 is X+2, X3 is X+3,
    Y1 is Y+1, Y2 is Y+2, Y3 is Y+3,
    eval(X,Y,E),eval(X1,Y1,E1),eval(X2,Y2,E2),eval(X3,Y3,E3),
    Ne  is E+1 ,retractall(eval(X,Y,E))  ,assert(eval(X,Y,Ne)),
    Ne1 is E1+1,retractall(eval(X1,Y1,E1)),assert(eval(X1,Y1,Ne1)),
    Ne2 is E2+1,retractall(eval(X2,Y2,E2)),assert(eval(X2,Y2,Ne2)),
    Ne3 is E3+1,retractall(eval(X3,Y3,E3)),assert(eval(X3,Y3,Ne3))
  ); true.

calcularDiagonalSecundaria(X,Y) :-
  ( X1 is X-1, X2 is X-2, X3 is X-3,
    Y1 is Y+1, Y2 is Y+2, Y3 is Y+3,
    eval(X,Y,E),eval(X1,Y1,E1),eval(X2,Y2,E2),eval(X3,Y3,E3),
    Ne  is E+1 ,retractall(eval(X,Y,E))  ,assert(eval(X,Y,Ne)),
    Ne1 is E1+1,retractall(eval(X1,Y1,E1)),assert(eval(X1,Y1,Ne1)),
    Ne2 is E2+1,retractall(eval(X2,Y2,E2)),assert(eval(X2,Y2,Ne2)),
    Ne3 is E3+1,retractall(eval(X3,Y3,E3)),assert(eval(X3,Y3,Ne3))
  ); true.