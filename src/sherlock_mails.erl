-module(sherlock_mails).

%% Copyright (c) 2009 Whoomph Software AB (joearms@gmail.com). All rights reserved.
%% For license read the top level LICENSE file

-compile(export_all).

-include("./sherlock.hrl").

test() ->
    %% this is a cmplete run of all the routines that are called
    get_index(),
    Years = find_mail_years(),
    io:format("Years = ~p~n", [Years]),
    parse_mails("2009"),
    compute_tfidf("2009"),
    add_synthetic_keywords("2009"),
    [N] = search_mails_regexps("2009", "*Armstrong*", "*JSON*", "*"),
    show_mail("2009", N),
    [H|_] = find_similar_mails("2009", "./src/sherlock_tfidf.erl"),
    show_mail("2009", H).

%% Given a File find the mail
%% in Year that best matches the content of the file
 
find_similar_mails(Year, File) ->
    Idf = filename:join([sherlock_root(),"mails",Year,"idf.ets"]),
    {ok, Tab} = ets:file2tab(Idf),
    Ks = sherlock_tfidf:keywords_in_file(File, Tab),
    Keywords = [K || {{_,K},_} <- Ks],
    Scores = [{Index,Score} || {{Index,_},Score} <- Ks],
    io:format("Searching for=~p~n",[Keywords]),
    ets:delete(Tab),
    Best = sherlock_best:init(10),
    File1 = filelib:wildcard(filename:join([sherlock_root(),"mails",
					   Year,"mails.bin"])),
    L = sherlock_lib:file2term(File1),
    Best1 = find_best_match(Scores, L, Best),
    show_best(Best1),
    %% now return the Id's
    [Id || {{Id,_},_} <- Best1].

%% print the scores
show_best([{{Id,Subject},Score}|T]) ->
    io:format("~w : ~4.2f ~s~n",[Id,Score,Subject]),
    show_best(T);
show_best([]) ->
    [].

find_best_match(K1, [#post{id=Id,scores=K2,subject=S}|T], Best) ->
    Score = sherlock_similar:cosine_similarity(K1, K2),
    Best1 = sherlock_best:add({Id,S}, Score, Best),
    find_best_match(K1, T, Best1);
find_best_match(_, [], Best) ->
    sherlock_best:final(Best).

show_mail(Year, Id) ->
    File = filelib:wildcard(filename:join([sherlock_root(),"mails",
					   Year,"mails.bin"])),
    L = sherlock_lib:file2term(File),
    Post = lists:nth(Id, L),
    pp(Post).

pp(#post{id=Id,subject=S,date=D,from=W,body=B}) ->
    io:format("----~nID: ~w~nDate: ~s~nFrom: ~s~nSubject: ~s~n~s~n",
	      [Id,D,W,S,B]).

search_mails_regexps(Year, Person, Subject, RegExp) ->
    %% Seach by Year, Then person, Subject, and RegExp
    File = filelib:wildcard(filename:join([sherlock_root(),"mails",
					   Year,"mails.bin"])),
    L = sherlock_lib:file2term(File),
    Reg1 = compile_awk_reg(Person),
    Reg2 = compile_awk_reg(Subject),
    Reg3 = compile_awk_reg(RegExp),
    {Time, Ids} = timer:tc(?MODULE, filter, [L, [Reg1,Reg2,Reg3], []]),
    io:format("Query took:~w ms #results=~w~n",[trunc(Time/1000), length(Ids)]),
    Ids.

filter([#post{subject=Subject,body=Body,from=Who, id=Id}|T], Regs, L) ->
    case matches([Who,Subject,Body], Regs) of
	true ->
	    io:format("~w:  ~s~n",[Id,Subject]),
	    filter(T, Regs, [Id|L]);
	false ->
	    filter(T, Regs, L)
    end;
filter([], _, L) ->
    lists:reverse(L).

matches([_Bin|T1], [match|T2]) ->
    matches(T1, T2);
matches([Bin|T1], [Reg|T2]) ->
    case re:run(Bin, Reg, [{capture,none}]) of
	match ->
	    matches(T1, T2);
	nomatch ->
	    false
    end;
matches([], []) ->
    true.

get_index() ->
    io:format("get index~n Input: none~n Output:$(MAIL)/questions.term~n"),
    ensure_mail_root(),
    Index       = filename:join([sherlock_root(),"mails","questions.html"]),
    ParsedIndex = filename:join([sherlock_root(),"mails","questions.term"]),
    sherlock_get_mails:get_index(Index),
    sherlock_get_mails:parse_index(Index, ParsedIndex).

parse_mails(Year) ->
    io:format("Parsing mails for: ~s~n",[Year]),    
    ensure_mail_year_directory(Year),
    Files = find_files_for_year(Year),
    L     = parse_compressed_mails(Files, 1, []),
    Out   = filename:join([sherlock_root(),"mails",Year,"parsed.bin"]),
    Bin   = term_to_binary(L, [compressed]),
    Size  = byte_size(Bin),
    ok    = file:write_file(Out, Bin),
    io:format("Written: ~s~n",[Out]),
    Len  = length(L),
    io:format("Year: ~s #entries = ~p size = ~6.2f Megabytes "
	      "average =~6.2f bytes/entry~n",[Year, Len, Size/1000000, Size/Len]).

parse_compressed_mails([File|T], N, L) ->
    {N1, L1} = sherlock_parse_mails:parse_compressed_mail_file(File, N, L),
    parse_compressed_mails(T, N1, L1);
parse_compressed_mails([], _, L) ->
    lists:reverse(L).
    
find_files_for_year(Year) ->
    Files  = filelib:wildcard(sherlock_cache() ++ "/" ++ Year ++"-*.gz"),
    Files1 = [filename:basename(I) || I <- Files],
    Months = [get_month(I) || I <- Files1],
    Months1 = sort_months(Months),
    Files2 = [ [Year ++ "-" ++ I] || I <- Months1],
    [filename:join([sherlock_cache(),I]) || I <- Files2].

get_month(File) ->
    [_,Month] = string:tokens(File,"-"),
    Month.

find_mail_years() ->
    Files  = filelib:wildcard(sherlock_cache() ++ "/*.gz"),
    Files1 = [filename:basename(I) || I <- Files],
    Years = [hd(string:tokens(I, "-")) || I <- Files1],
    lists:sort(remove_duplicates(Years)).

normalise_months(Year, L) ->
    L1 = sort_months(L),
    io:format("L1=~p~n",[L1]),
    [Year ++ "-" ++ I || I <- L1].

sort_months(L) ->
    F = fun(File) ->
		[Month,_,_] = string:tokens(File, "."),
		{sherlock_lib:month2index(Month), File}
	end,
    L1 = [F(I) || I <- L],
    L2 = lists:sort(L1),
    %% remove the index
    [File || {_Index,File} <- L2].

compute_tfidf(Year) ->
    io:format("Computing mail IDF for: ~s~n", [Year]),
    In  = filename:join([sherlock_root(),"mails",Year,"parsed.bin"]),
    Out = filename:join([sherlock_root(),"mails",Year,"idf.ets"]),
    L   = sherlock_lib:file2term(In),
    %%
    Idf  = sherlock_tfidf:start(),
    Idf1 = loop1(L, Idf),
    Tab  = sherlock_tfidf:final(Idf1),
    %% uncomment the next line for debugging
    %% sherlock_lib:show_ets("idf.ets", Tab),
    ets:tab2file(Tab, Out),
    ets:delete(Tab).

loop1([Post|T], Idf) ->
    Bin = make_stuff_to_index(Post),
    Idf1 = sherlock_tfidf:update(Idf, Bin),
    loop1(T, Idf1);
loop1([], Idf) ->
    Idf.

make_stuff_to_index(#post{subject=S,body=B,from=F}) ->
    <<S/binary," ",B/binary," ", F/binary>>.

ensure_mail_root() ->
    file:make_dir(filename:join([sherlock_root(),"mails"])),
    file:make_dir(filename:join([sherlock_root(),"mails","cache"])).

sherlock_root() ->
    filename:join([os:getenv("HOME"),".sherlock"]).

sherlock_cache() ->
    filename:join([sherlock_root(),"mails","cache"]).

ensure_mail_year_directory(Year) ->
    Dir = filename:join([sherlock_root(),"mails",Year]),
    file:make_dir(Dir).

remove_duplicates(L) ->
    remove_duplicates(lists:sort(L), []).

remove_duplicates([H|X=[H|_]], L) -> remove_duplicates(X, L);
remove_duplicates([H|T], L)       -> remove_duplicates(T, [H|L]);
remove_duplicates([], L)          -> lists:reverse(L).

add_synthetic_keywords(Year) ->
    io:format("Adding synthetic keywords for:~s~n",[Year]),
    Idf = filename:join([sherlock_root(),"mails",Year,"idf.ets"]),
    {ok, Tab} = ets:file2tab(Idf),
    File = filename:join([sherlock_root(),"mails",Year,"parsed.bin"]),
    Out  = filename:join([sherlock_root(),"mails",Year,"mails.bin"]),
    List  = filename:join([sherlock_root(),"mails",Year,"mails.list"]),
    L   = sherlock_lib:file2term(File),
    L1  = [add_keywords(I, Tab) || I<- L],
    file:write_file(Out, term_to_binary(L1, [compressed])),
    io:format("Written binary store:~s~n",[Out]),
    sherlock_lib:term2file(L1, List),
    io:format("Written listing:~p~n",[List]).

add_keywords(Post, Tab)->
    B = make_stuff_to_index(Post),
    K = sherlock_tfidf:keywords_in_binary(B, Tab),
    %% K = [{{Index,Word},Sig}]
    Scores = [{Index,Sig} || {{Index,_}, Sig} <- K],
    Words  = [Word        || {{_,Word}, _} <- K],
    Words1 = list_to_binary(interlieve(Words,$,)),
    Post#post{keywords=Words1, scores=Scores}.

interlieve([_]=X, _) -> X;
interlieve([H|T], S) -> [H,S,interlieve(T,S)]; 
interlieve([], _)    -> []. 
    
compile_awk_reg("*") ->
    match;
compile_awk_reg(Reg) ->
    Reg1 = xmerl_regexp:sh_to_awk(Reg),
    {ok, Reg2} = re:compile(Reg1, [caseless,unicode,dotall]),
    Reg2.
