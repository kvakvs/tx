# Term Explorer (TX)

A simple application to be used when debugging your apps. Given an arbitrary long nested (or 
simple) term, saves it in memory and creates an unique URL to it (similar to Pastebin but 
with faster expiration). Opening the URL displays a page with navigable tree-like term
structure. Data is released automatically for garbage collection, or when you click release
button.

# Example

X = erlang:process_info(self()).
tx:tx(X).
