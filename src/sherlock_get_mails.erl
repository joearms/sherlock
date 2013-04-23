-module(sherlock_get_mails).

%% Copyright (c) 2009 Whoomph Software AB (joearms@gmail.com). All rights reserved.
%% For license read the top level LICENSE file

-export([get_index/1, parse_index/2, fetch_files/2]).

%% step 1) get_index()
%%         => index.tmp
%%      2) parse_index0()
%%         => urls.tmp
%%      3) fetch all the urls

get_index(File) ->
    Url = "http://erlang.org/pipermail/erlang-questions/",
    start_inets(),
    {ok, {_Status,_Headers,Body}} = httpc:request(Url),
    file:write_file(File, Body),
    io:format("Written: ~s~n",[File]).
    
start_inets() ->
    application:start(inets),
    %% Set the proxy (if used) in the next line
    %% Edit Proxy and Port to appropriate values
    %% httpc:set_options([{proxy,{{Proxy,Port},[]}}]),
    true.

parse_index(In, Out) ->
    {ok, Bin} = file:read_file(In),
    Tree  = mochiweb_html:parse(Bin),
    Rows  = findall([<<"html">>,<<"body">>,<<"table">>,<<"tr">>], Tree),
    Rows1 = [lists:nth(3,I) || {_,I} <- Rows],
    Rows2 = [findall([<<"td">>,<<"a">>], I) || I <- Rows1],
    Rows3 = [Url || [{[{<<"href">>,Url}],_}] <- lists:reverse(Rows2)],
    sherlock_lib:term2file(Rows3, Out),
    io:format("Written: ~s~n",[Out]),
    Rows3.

findall(Path, Tree) ->
    L1 =findall(Tree, lists:reverse(Path), [], []),
    lists:reverse(L1).

findall({Tag,A,C}, [Tag|Path], Path, L) ->
    [{A,C}|L];
findall({Tag,_,C}, Want, Path, L) ->
    findall(C, Want, [Tag|Path], L);
findall([H|T], Want, Path, L) ->
    L1 = findall(H, Want, Path, L),
    findall(T, Want, Path, L1);
findall(_,_,_,L) ->
    L.

%% Exercise improve findall
%% by limiting the depth of the search the 2'nd clause should not 
%% recurse into findall if the depth of the path is 
%% greater that the required term

%% fetch all files in the list of urls

fetch_files(In, Cache) ->
    start_inets(),
    {ok, [Urls]} = file:consult(In),
    io:format("Urls=~p~n",[Urls]),
    [fetch_data(I, Cache) || I <- Urls].

fetch_data(I, Cache) ->
    S = binary_to_list(I),
    Out = filename:join([Cache,S]),
    io:format("testing:~p~n",[Out]),
    case filelib:is_regular(Out) of
	true ->
	    io:format("~p is in the cache~n",[I]);
	false ->
	    fetch_and_store(S, Out)
    end.

fetch_and_store(F, Out) ->
    Url = "http://erlang.org/pipermail/erlang-questions/" ++ F,
    io:format("fetching:~p~n",[Url]),
    {ok, {_Status,_Headers,Body}} = httpc:request(Url),
    file:write_file(Out,Body),
    io:format("written:~s~n",[Out]).

%% Actually what we have done is build what is essentially
%% a mico version of XPATH that works on the parse trees produced 
%% my mochiweb_html. Our findall is an xpath query of the form
%% "html/body/table/*"

%% Exercise: ...





