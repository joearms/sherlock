-module(sherlock_best).

%% Copyright (c) 2009 Whoomph Software AB (joearms@gmail.com). All rights reserved.
%% For license read the top level LICENSE file

-export([init/1, add/3, final/1]).

init(N) -> {container, N, []}.

add(File, Score, {container,N,L}) ->
    L1 = inject(File, Score, L),
    L2 = trim(N, L1),
    {container, N, L2}.

inject(File, Score, [{F,S}|T]) when S > Score -> [{F,S}|inject(File, Score, T)];
inject(File, Score, T)                        -> [{File,Score}|T].

trim(0, _)     -> [];
trim(_, [])    -> [];
trim(N, [H|T]) -> [H|trim(N-1,T)].
    
final({container,_,L}) -> L.
