.PHONY: shell
shell:
	rebar compile && erl -pa ./ebin/ deps/*/ebin -s tx_app

.PHONY: test
test:
	rebar eunit


