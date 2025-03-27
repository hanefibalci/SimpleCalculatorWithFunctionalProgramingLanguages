:- initialization(main).
:- use_module(library(dcg/basics)).

% Operatör önceliklerini tanımla
:- op(500, yfx, [+, -]).
:- op(400, yfx, [*, /]).

main :-
    writeln('Prolog Hesap Makinesi (Çıkmak için "exit" yazın)'),
    calculation_loop.

calculation_loop :-
    write('>>> '),
    flush_output,
    read_line_to_string(user_input, Input),
    (   Input = "exit"
    ->  writeln('Çıkılıyor...')
    ;   process_input(Input),
        calculation_loop
    ).

process_input(Input) :-
    string_chars(Input, Chars),  % String'i karakter listesine çevir
    (   phrase(parse_expr(Expr), Chars)
    ->  (   catch(eval(Expr, Result), Error, error_handler(Error))
        ->  format('Sonuç: ~w~n', [Result])
        ;   writeln('Hesaplama hatası!'))
    ;   writeln('Geçersiz ifade!')
    ).

error_handler(Error) :-
    write('Hata: '),
    (   Error = error(evaluation_error(zero_divisor), _)
    ->  writeln('Sıfıra bölme!')
    ;   writeln(Error)
    ),
    fail.

% DCG ile ifade ayrıştırma (güncellendi)
parse_expr(Expr) --> parse_term(T1), parse_expr_tail(T1, Expr).

parse_expr_tail(T1, Expr) -->
    [Op], {member(Op, ['+', '-'])}, parse_term(T2), {combine(Op, T1, T2, T3)}, parse_expr_tail(T3, Expr).
parse_expr_tail(Expr, Expr) --> [].

parse_term(Expr) --> parse_factor(F1), parse_term_tail(F1, Expr).

parse_term_tail(F1, Expr) -->
    [Op], {member(Op, ['*', '/'])}, parse_factor(F2), {combine(Op, F1, F2, F3)}, parse_term_tail(F3, Expr).
parse_term_tail(Expr, Expr) --> [].

parse_factor(N) --> number(N).  % Doğrudan number DCG'sini kullan
parse_factor(Expr) --> ['('], parse_expr(Expr), [')'].

combine('+', A, B, A + B).
combine('-', A, B, A - B).
combine('*', A, B, A * B).
combine('/', A, B, A / B).

eval(N, N) :- number(N).
eval(A + B, R) :- eval(A, A1), eval(B, B1), R is A1 + B1.
eval(A - B, R) :- eval(A, A1), eval(B, B1), R is A1 - B1.
eval(A * B, R) :- eval(A, A1), eval(B, B1), R is A1 * B1.
eval(A / B, R) :- eval(A, A1), eval(B, B1), B1 =\= 0, R is A1 / B1.