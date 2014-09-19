.PHONY: shell
shell:
	rebar compile && erl -pa ./ebin/ -s tx_app

