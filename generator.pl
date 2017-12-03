%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Generator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Generates an equation using an environment
% generate_equation(+Env, -Eq)
generate_eq(Env, Eq) :-
       % Create equation
       generate_eq(Env, [], Eq).

% Generates a solvable equation based on a list of previous equations and those equations shared environment
% generate_equation(+Env, +Eq_list, -Eq)
generate_eq(Env, Eq_list, [Ans,=|Expr_simple]) :-
        create_expr(Env, 8, 1, [], [+|Expr_long]),
        parse(Expr_long, Env, Expr_short),
        eq_sys(Eq_list, Env, Expr_short, Eq_sys),
        gauss(Eq_sys),
        solve(Expr_short, Env, 0, Ans),
        readable(Expr_short, Expr_simple).
        
% Creates a long expression
% create_expr(+Env, +Count, +Co, +Expr_acc, -Expr)
create_expr(_,  0, _, Expr, Expr).
create_expr(Env, Count, Co0, Expr_acc, Expr) :-
        Count =\= 0,
        % Generate a coefficient and a variable
        coefficient(Co0,New_co),
        variable(Env, [Var, _]),
        Dec_count is Count - 1,
        % The increase if to give more fruitful answers
        Inc_co is (Co0 + 1)mod(7),
        create_expr(Env, Dec_count, Inc_co, [+,New_co,Var|Expr_acc], Expr).

% Gives all possible coefficients, using math to give more fruitful answers
% coefficient(+Co, -Chosen)
coefficient(Co, Co).
coefficient(Co0, Co1) :-
        Co1 is -1*Co0.
coefficient(Co, Chosen) :-
        New_co is (Co+7),
        New_co =< 17, % 17 is chosen max, nothing special
        coefficient(New_co, Chosen).

% Gives all possible variables or a constant
% varialbe(+List, -Chosen)
variable([Chosen|_], Chosen).
variable([_|Tail], Chosen) :-
        variable(Tail, Chosen).
variable([], [const, _]). %Final should be constant

% Checks if equations are solvable with the environment and creates an matrix from the expressions
% eq_sys(+Eq_list, +Env, +New_expr, -Eq_sys)
eq_sys([], _, Expr, [Expr]).
eq_sys([[Ans,=,-|Expr]|Eqs], Env, New_expr, [Old_expr|Old_exprs]) :-
        % Change equation to a more workable form
        reverse_simplify([-|Expr], Env, Old_expr),
        solve(Expr, Env, 0, Ans), 
        eq_sys(Eqs, Env, New_expr, Old_exprs).
eq_sys([[Ans,=,Op|Expr]|Eqs], Env, New_expr, [Simple_expr|Simple_exprs]) :-
        Op \== -,
        reverse_simplify([+,Op|Expr], Env, Simple_expr),
        solve(Simple_expr, Env, 0, Ans),
        eq_sys(Eqs, Env, New_expr, Simple_exprs).

% Solves an expression using the environment
% solve(+Expr, +Env, +Ans_acc, ?Ans)
solve([[const, Co]], _, Ans_acc, Ans) :- Ans is Ans_acc+Co, !. %Green cut, removes a guaranteed to fail lookup
solve([[Var, Co]|Rest], Env, Ans_acc, Ans) :-
        lookup(Env, Var, Val),
        solve(Rest, Env, Ans_acc+Co*Val, Ans).

% Decides if system is solvable using the first steps of Gauss elimination
% gauss(+Eq_sys)
gauss([]).
gauss([[[_, Val]|Row]|Gauss0]) :-
        Val =\= 0,
        gauss_rowdiv(Row, Val, [[_, 1]], Row1),
        gauss_div(Gauss0, [], Gauss1), % Will reverse list
        gauss_sub(Gauss1, Row1, [], Gauss2), % Will re-reverse list
        gauss(Gauss2).

% Performs the division step of Gauss elimination
% gauss(+Eq_sys, +Acc, +Result)
gauss_div([], Result, Result).
gauss_div([[[_, Co]|Rest]|Rows], Div_acc, Result) :-
        Co =\= 0,
        gauss_rowdiv(Rest, Co, [[_, 1]], New_Row),
        gauss_div(Rows, [New_Row|Div_acc], Result).

% Divides all elements in a vector with a denominator
% gauss_rowdiv(+Row, +Denom, -Result)
gauss_rowdiv([[const, _]], _, Result, [[const, _]|Result]) :- !. %Red cut, const is always end of list
gauss_rowdiv([[_, Co]|Rest], Denom, Row_acc, Result) :-
        New_co is Co / Denom,
        gauss_rowdiv(Rest, Denom, [[_, New_co]|Row_acc], Result).

% Subtracts one vector on all vectors in a system
% gauss_sub(+Eq_sys, +Acc, -Result)
gauss_sub([], _, Result, Result).
gauss_sub([[_|Row]|Rows], [_|Subtnd], Sub_acc, Result) :- % '_' is always const
        gauss_rowsub(Row, Subtnd, [[const, _]], Diff), % Bring back const at end of list
        gauss_sub(Rows, [_|Subtnd], [Diff|Sub_acc], Result).

% Subtracts one vector with another
% gauss_rowsub(+Minnd, +Subtnd, +Acc, -Result)
gauss_rowsub([[_, _]], [[_, _]], Result, Result). % Reversed from div, final difference is always 1 - 1
gauss_rowsub([[_, Minnd]|Rest1], [[_, Subtnd]|Rest2], Row_acc, Results) :-
        Rest1 \== [], % As long as we are not on the final element we end up here
        Diff is Minnd - Subtnd,
        gauss_rowsub(Rest1, Rest2, [[_, Diff]|Row_acc], Results).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PARSER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Makes an expression readable
% readable(+Expr, -Eq)
readable(Expr, Expr_simple) :-
        simplify(Expr, Expr_ans),
        % first_sym() avoids having to redo simplify if we do not get expected result.
        first_sym(Expr_ans, Expr_simple).

% Removes '+' if it is the first symbol in the list
% first_sym(+Expr, -Expr_done)
first_sym([+|Expr], Expr).
first_sym([-|Expr], [-|Expr]).

% Simplifies an expression for human readability
% simplify(+Expr, -Expr_simple)
simplify([[const, 0]], []).
simplify([[const, Const]], [+,Const]) :-
        Const > 0.
simplify([[const, Const0]], [-,Const]) :-
        Const0 < 0,
        Const is -1*Const0.
simplify([[_, 0]|Expr], Expr_sofar) :-
        Expr \== [],
        simplify(Expr, Expr_sofar).
simplify([[Var, Co]|Expr], [+,Co,Var|Expr_sofar]) :-
        Co > 1,
        Expr \== [],
        simplify(Expr, Expr_sofar).
simplify([[Var, Co0]|Expr], [-,Co,Var|Expr_sofar]) :-
        Co0 < -1,
        Expr \== [],
        Co is -1*Co0,
        simplify(Expr, Expr_sofar).
simplify([[Var, 1]|Expr], [+,Var|Expr_sofar]) :-
        Expr \== [],
        simplify(Expr, Expr_sofar).
simplify([[Var, -1]|Expr], [-,Var|Expr_sofar]) :-
        Expr \== [],
        simplify(Expr, Expr_sofar).
      
% Reverses what simplify does, making a human readable expression into a workable expression for the program
% reverse_simplify(+Expr_simple, +Env, -Result)
reverse_simplify([], [], [[const, 0]]).
reverse_simplify([-, Co], [], [[const, Co1]]):- Co1 is -1*Co.
reverse_simplify([+, Co], [], [[const, Co]]).
  
reverse_simplify([S, Co, Var1|Eq], [[Var2, _]|Env], [[Var2, 0]|Var_list]):-
        integer(Co), 
        Var1 \== Var2,
        reverse_simplify([S, Co, Var1|Eq], Env, Var_list).
reverse_simplify([+,Var|Eq], [[Var,_]|Env], [[Var, 1]|Var_list]):-
        reverse_simplify(Eq, Env, Var_list).
reverse_simplify([-,Var|Eq], [[Var,_]|Env], [[Var, -1]|Var_list]):-
        reverse_simplify(Eq, Env, Var_list).
reverse_simplify([+,Co, Var|Eq], [[Var,_]|Env], [[Var, Co]|Var_list]):-
        integer(Co),   
        reverse_simplify(Eq, Env, Var_list).
reverse_simplify([-,Co, Var|Eq], [[Var,_]|Env], [[Var, Co1]|Var_list]):-
        integer(Co), 
        Co1 is -1*Co,
        reverse_simplify(Eq, Env, Var_list).   
reverse_simplify([S,Var|Eq], [[Var1,_]|Env], [[Var1, 0]|Var_list]):-
        \+ integer(Var), 
        Var \== Var1,
        reverse_simplify([S,Var|Eq], Env, Var_list).     
reverse_simplify([], [[Var2, _]|Env], [[Var2, 0]|Var_list]):-
        reverse_simplify([], Env, Var_list).
        
% looks up variable value from environment
% lookup(+Env, +Var, -Val)
lookup([[Var, Val]|_], Var, Val) :- !. %Green cut, guaranteed to not find another answer in env
lookup([[_, _]|Rest], Var, Val) :-
        lookup(Rest, Var, Val).

% Parse parses a list with an equation to add all terms with the same variable together
% parse(+Expr, +Env, -Parsed)
parse(Expr, Env, Parsed) :-
        create_var_list(Env, Var_list),
        gather(Expr, Var_list, Parsed).


% create_var_list creates a list based on the environment that will contain the coefficients for each terms
% create_var_list(+Env, -Var_list) 
create_var_list([],[[const, 0]]).
create_var_list([[X,_]|T], [[X,0]|Rear]):- create_var_list(T, Rear).

% Gather all the terms with the same variable together
% gather(+Expr, +Var_list, -Result)
gather([], Result, Result). 
gather([N1, X, +|Expr], Var_list, Result):-
        fills(Var_list, N1, X, New_var),  %Add number N1 to the corresponding var X in Var_list
        gather(Expr, New_var, Result).
gather([N1, X], Var_list, Result):-
        fills(Var_list, N1, X, Result).  %Add number N1 to the corresponding var X in Var_list
    
% Add number N1 to the corresponding var X in Var_list   
% fills(+Var_list, +Val, +Var, -New_var)              
fills([],_,_,[]).         
fills([[X, Val]|Tail], N1, X, [[X, Y]|Acc]):-
        Y is N1 + Val,
        fills(Tail, N1, X, Acc).                      

fills([[Var, Val]|Tail], N1, X, [[Var, Val]|Acc]):- 
        X \== Var,
        fills(Tail, N1, X, Acc).
