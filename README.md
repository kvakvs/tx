# Erlang Term Explorer (TX)

This app was created to fill the void in the data visualisation area. And to 
help you follow "know your data" paradigm. Basically it shows you your data in 
web browser via webserver listening to localhost. 

## What does it look like?

<img src="https://raw.github.com/kvakvs/tx/master/priv/img/readme_index.png" width="30%" />
<img src="https://raw.github.com/kvakvs/tx/master/priv/img/readme_show.png" width="30%" />
<img src="https://raw.github.com/kvakvs/tx/master/priv/img/readme_inspect_proplist.png" width="30%" />

## Features 

*  `tx:show(Term)` and `tx:show(Term, Title)`
   *  Given an arbitrary term, saves it temporarily and creates an unique 
      URL to it (similar to Pastebin but with faster/manual expiration). 
      Click the URL or copy and open in your web browser (Javascript required).
*  `tx:redbug(Spec)` and `tx:redbug(Spec, Options)`
   *  If you have [Redbug](http://github.com/massemanet/eper/) available, then
      tx will run `redbug:start` and capture events output, then store it in a
      regular way as a browsable term.
*  Process and ports are "inspectable", just click one to see. 
*  Proplists with printable keys are detected and formatted tidier.
*  Uses only standard Erlang libraries (inets), no external dependencies.

## How to begin

Add `https://github.com/kvakvs/tx` as your application dependency or download it
and compile manually. Run your application as usual. When you need to see things,
call `tx:show(Term).` in Erlang console or insert call to `tx:show` where you 
need it. URL is returned from `tx:show`, and Erlang shell will display it. NOTE:
return value won't be displayed if you call `tx:show` in your code.

Application `tx` will be started automatically and web server will be opened on
your first call of `tx:show/1`. Alternatively run tx manually via: `tx:start().`.
Default port is 20000, and default host is `localhost`. You can set port to 0 in 
tx.app.src, to start on random available port.


## Example

Simple demo: `tx:show(application:which_applications()).`

All sorts of terms in one: `tx:show([<<123,32,1>>, <<1:1, 3:5, 45:7>>, <<"test\ttest">>, 123, 123.321, 
make_ref(), self(), erlang:ports(), fun()->ok end, {fun erlang:ports/0, 'test', 
"test"}, [{test, value1}, {fgsfds, 1000}], [1,2,3|{4, [5|test]}]]).`

R17 maps example: `tx:show(#{k1 => "test", k2 => 'test', k3 => 123, 
k4 => <<"test">>}).`

## TODO

Possible things to add of improve

* Support compact display of value-only tuples and lists (multiple values per line)
* ? Backend could recognize record definitions and supply field information to 
  frontend
* Paste term from clipboard
* Implement automatic expiration (cleanup loop on timer)
* Toolbox for External Term Format: encode, decode, compress, view in hex etc.
* **Unfold the whole thing into introspection tool like tv (table viewer) or 
  observer.**
