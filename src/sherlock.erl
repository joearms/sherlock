-module(sherlock).
-compile(export_all).

init() ->
    io:format("Checking ${HOME}/.sherlock/mails~n"),
    sherlock_mails:ensure_mail_root().

fetch_index() ->
    sherlock_mails:get_index().

fetch_mails() ->
    sherlock_mails:fetch_all_mails().

mail_years() ->
    sherlock_mails:find_mail_years().

process_year(Year) ->
    N = sherlock_mails:process_year(Year),
    io:format("~s had ~w mails~n",[Year,N]),
    N.

process_all_years() ->
    T1 = erlang:now(),
    L = [process_year(I) || I <- mail_years()],
    T2 = erlang:now(),
    N = lists:sum(L),
    Tdiff = tdiff(T1, T2),
    io:format("~w files in ~w seconds (~5.1f files/second)~n",[N,Tdiff, N/Tdiff]).

tdiff({Mega1,S1,_},{Mega2,S2,_}) ->
    (Mega2-Mega1)*1000000 + S2 - S1.

search_mails_regexprs(Year, Who, Subject, Body) ->
    sherlock_mails:search_mails_regexprs(Year, Who, Subject, Body).

print_mail(Year, Id) ->
    sherlock_mails:print_mail(Year, Id).

find_mails_similar_to_mail(SearchYear, MailYear, MailId) ->
    sherlock_mails:find_mails_similar_to_mail(SearchYear, MailYear, MailId).

find_mails_similar_to_file(Year, File) ->
    sherlock_mails:find_mails_similar_to_file(Year, File).

get_keyword_vector(Year, Id) ->
    sherlock_mails:get_keyword_vector(Year, Id).

cosine_similarity(Year, Id1, Id2) ->
    K1 = sherlock_mails:get_keyword_vector(Year, Id1),
    K2 = sherlock_mails:get_keyword_vector(Year, Id2),
    Sim = sherlock_similar:cosine_similarity(K1, K2),
    {K1, K2, Sim}.


