all:
	rebar compile
	erl -pa ./ebin -s sherlock_mails test -s init stop > LOG




