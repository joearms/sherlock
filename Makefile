test_search:
	rebar compile
	erl -pa ./ebin -s sherlock_mails test_search -s init stop > LOG_test

test_build:
	rebar compile
	erl -pa ./ebin -s sherlock_mails test_build -s init stop > LOG_build






