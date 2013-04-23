-module(sherlock_tfidf).

%% Copyright (c) 2009 Whoomph Software AB (joearms@gmail.com). All rights reserved.
%% For license read the top level LICENSE file

-export([start/0, update/2, final/1, 
	 keywords_in_file/2,
	 extract_all_words_in_file/1,
	 keywords_in_binary/2
	]).
	 
start() ->
    Tab = ets:new(tmp, [set]),
    {Tab, 0}.

update({Tab, N}, Bin) ->
    %% find all the words in the document
    Words  = bin_to_words(Bin),
    Words1 = remove_duplicates(Words),
    [update_counter(Word, Tab) || Word <- Words1],
    {Tab, N+1}.

final({Tab, Tot}) ->
    IDF = ets:new(idf, [set]),
    ets:foldl(fun({Word,Count}, Index) ->
		      Idf = math:log(Tot/(Count+1)),
		      ets:insert(IDF, {Word,Index,Idf}),
		      Index+1
	      end, 1, Tab),
    ets:delete(Tab),
    IDF.

%% compute_keywords(Files) ->
%%     IDF = compute_idf(Files),
%%     Keywords = [keywords_in_file(File, IDF) || File <- Files],
%%     {Keywords, IDF}.

keywords_in_file(File, IDF) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    keywords_in_binary(Bin, IDF);
	{error, []} ->
	    []
    end.

keywords_in_binary(Bin, IDF) ->
    TF = term_frequency(Bin),
    Tot = length(TF),
    Scores = [tfidf(Word, N, Tot, IDF) || {Word, N} <- TF],
    Scores1 = lists:reverse(lists:keysort(2, Scores)),
    Scores2 = trim(Scores1, 15),
    Scores3 = remove_small(Scores2),
    %% io:format("Scores for : ~p~n",[Scores3]),
    %% Len1 = byte_size(term_to_binary(Scores3)),
    %% Len2 = byte_size(term_to_binary(Scores3, [compressed])),
    %% Size = filelib:file_size(File),
    %% io:format("~p size:~p data:~p ~p~n",[File,Size,Len1,Len2]),
    Scores3.

%% compute_idf(Files) ->
%%     WordCounts = ets:new(tmp, [set]),
%%     [process(I, WordCounts) || I <- Files],
%%     L = ets:tab2list(WordCounts),
%%     %% L is a list [{<<"word1">>, 12},<<"module">>, 15}]
%%     io:format("L=~p~n",[L]),
%%     %% Now compute the IDF
%%     Tot = length(Files),
%%     IDF = ets:new(idf, [set]),
%%     ets:foldl(fun({Word,N}, _) ->
%% 		      Idf = math:log(Tot/(N+1)),
%% 		      ets:insert(IDF, {Word,Idf}),
%% 		      []
%% 	      end, [], WordCounts),
%%     ets:delete(WordCounts),
%%     io:format("Idf=~p~n",[ets:tab2list(IDF)]),
%%     ets:tab2file(IDF, "idf.ets"),
%%     IDF.
    

%% IDF = Inverse Document Frequency
%%     = log(Tot/N+1)

%%  idf(Word) = log(Tot/N+1)
%%  Tot = Total number of documents
%%  N = the number of documents that contain the word W

%% we create an ets table

extract_all_words_in_file(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    bin_to_words(Bin);
	{error, _} ->
	    []
    end.

-spec bin_to_words(binary()) -> [Word::binary()].

bin_to_words(Bin) ->
    {ok, Words} = text_analyzers:standard_analyzer_factory(Bin, ["3"]),
    remove_skip(Words).

%% The text_analyser includes the special token skip
%% if a skipped word is included in the word list
%% we want to remove this

remove_skip([skip|T]) -> remove_skip(T);
remove_skip([H|T])    -> [H|remove_skip(T)];
remove_skip([])       -> [].

update_counter(Word, Tab) ->
    case ets:lookup(Tab, Word) of
	[{Word,N}] ->
	    ets:insert(Tab, {Word,N+1});
	[] ->
	    ets:insert(Tab, {Word,1})
    end.


%%----------------------------------------------------------------------

-spec term_frequency(B::binary()) -> [{Word::binary(), Freq::float()}].

term_frequency(Bin) ->
    Words = bin_to_words(Bin),
    %% Note this time we don't remove the duplicates
    D1 = lists:foldl(fun count_words/2, dict:new(), Words),
    dict:to_list(D1).

tfidf(Word, N, Tot, IDFTab) ->
    TF = N/Tot,
    {Index, Idf} = idf(Word, IDFTab),
    {Index, TF*Idf}.

idf(Word, Tab) ->
    case ets:lookup(Tab, Word) of
	[{Word,Index,IDF}] -> {{Index,Word}, IDF};
	_                  -> {0,0}
    end.

count_words(Word, D0) ->
    case dict:find(Word, D0) of
	{ok, N} -> dict:store(Word, N+1, D0);
	error   -> dict:store(Word, 1, D0)
    end.

%% trim(L) takes the first N elements of
%% L if length(L) > N otherwsie returns L

trim(L, N) ->
    string:substr(L, 1, N).

remove_small([{_Word,Score}|_]) when Score < 0.1 ->  [];
remove_small([H|T])                              ->  [H|remove_small(T)];
remove_small([])                                 ->  [].
    
remove_duplicates(L) ->
    remove_duplicates(lists:sort(L), []).

remove_duplicates([H|X=[H|_]], L) -> remove_duplicates(X, L);
remove_duplicates([H|T], L)       -> remove_duplicates(T, [H|L]);
remove_duplicates([], L)          -> L.

