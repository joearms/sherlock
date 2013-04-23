-module(sherlock_lib).

%% Copyright (c) 2009 Whoomph Software AB (joearms@gmail.com). All rights reserved.
%% For license read the top level LICENSE file

-export([file2term/1, term2file/2, show_ets/2, merge_kv/1, month2index/1]).

file2term(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_term(Bin).

term2file(Term, File) ->
    {ok, S} = file:open(File, [write]),
    ok = io:format(S, "~p.~n",[Term]),
    file:close(S).

%%----------------------------------------------------------------------
%% @doc
%% Take a property list [{Key,Val}] where Key can occur
%%    More than once and make it into a list {Key, [Val]} where
%%    each Key occurs only once.

-spec merge_kv(Kv::[{Key,Val}]) -> Kv1::[{Key,[Val]}].

merge_kv(KV) ->  merge_kv(KV, dict:new()).

merge_kv([{Key,Val}|T], D0) ->
    case dict:find(Key, D0) of
	{ok, L} -> merge_kv(T, dict:store(Key, [Val|L], D0));
	error   -> merge_kv(T, dict:store(Key, [Val], D0))
    end;
merge_kv([], D) ->
    dict:to_list(D).

month2index("January")   -> 1;
month2index("February")  -> 2; 
month2index("March")     -> 3; 
month2index("April")     -> 4;
month2index("May")       -> 5; 
month2index("June")      -> 6;
month2index("July")      -> 7;
month2index("August")    -> 8; 
month2index("September") -> 9; 
month2index("October")   -> 10; 
month2index("November")  -> 11;
month2index("December")  -> 12.
    
%%----------------------------------------------------------------------
%% show the first few entries in an ETS table
%% used for debugging purposes

show_ets(Str, Tab) ->
    io:format("~s :: first few entries are ...~n",[Str]),
    Key = ets:first(Tab),
    show(5, Str, Tab, Key).

show(0,_,_,_) -> void;
show(_,_,_,'$end_of_table') ->
    void;
show(N, Str, Tab, Key) ->
    Val = ets:lookup(Tab, Key),
    io:format("~s :: ~p => ~p~n",[Str,Key,Val]),
    show(N-1, Str, Tab, ets:next(Tab, Key)).




