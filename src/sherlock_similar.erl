-module(sherlock_similar).

%% Copyright (c) 2009 Whoomph Software AB (joearms@gmail.com). All rights reserved.
%% For license read the top level LICENSE file

-export([cosine_similarity/2, cross/2, norm/1]).

%%----------------------------------------------------------------------
%% compute the consine similarity of two vectors
%% L1 , L2 of the form [{Term,Frac}]
%% computes the cross product ..

cosine_similarity([], _)  -> 0;
cosine_similarity(_, [])  -> 0;
cosine_similarity(L1, L2) -> cross(L1, L2)/(norm(L1)*norm(L2)).

cross(L1, L2) ->
    D = dict:from_list(L1),
    Prods = [Wt*weight(Index, D) || {Index, Wt} <- L2],
    lists:sum(Prods).

weight(Word, D) ->
    case dict:find(Word, D) of
	{ok, Wt} -> Wt;
	error    -> 0
    end.

%% http://en.wikipedia.org/wiki/Vector_space_model
%% Norm of the vector is ...

norm(L) ->
    L1 = [Weight*Weight || {_,Weight} <- L],
    L2 = lists:sum(L1),
    %% io:format("L2=~p~n",[L2]),
    math:sqrt(L2).


