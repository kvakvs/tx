# Erlang Term Explorer (TX)

This app was created to fill the void in the data visualisation area. And to 
help you follow "know your data" paradigm. Basically it shows you your data in 
web browser via webserver listening to localhost. 

Given an arbitrary term, saves it temporarily and creates an unique 
URL to it (similar to Pastebin but with faster/manual expiration). Clicking the
URL in your console window will open web browser and load a page with tree-like 
term structure.

Uses only standard Erlang libraries (inets), no external dependencies.

# Running

run manually via: `application:start(tx).` or it will be started automatically
if you forget to do that. Default port is 20000, and default host is localhost.

# Example

    X = erlang:process_info(self()).
    tx:show(X). 
