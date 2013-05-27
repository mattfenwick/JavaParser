Tokenization:

 - `int doublev = 3;` is valid according to the Eclipse and ideone.com compilers --
   that is, `double` is not parsed as a keyword, leaving `v = 3;`.
   Is this because of the longest-match tokenization rule?

 - deciding between keyword/identifier/null/boolean:  it looks like you have to take 
   the longest match that fits the `/[a-zA-Z][a-zA-Z0-9$_]*/` pattern, and then 
   classify it into which type of token it is; you can't go with a shorter match even
   if it would save a failing parse.  i.e. `doublev ` should be parsed as the identifier
   `doublev` with a space left over, not as the keyword `do` or `double`.
   
 - `\n`, `\r`, and `\r\n` are the line terminators, apparently, and `\f` is a whitespace character

 - remember to use `InputCharacter`, not `item`, to consume a single *Java* character, since Java
   has the stream undergo unicode something-or-other.