Tokenization:

 - `int doublev = 3;` is valid according to the Eclipse and ideone.com compilers --
   that is, `double` is not parsed as a keyword, leaving `v = 3;`.
   Is this because of the longest-match tokenization rule?

 - deciding between keyword/identifier/null/boolean:  it looks like you have to take 
   the longest match that fits the `/[a-zA-Z][a-zA-Z0-9$_]*/` pattern, and then 
   classify it into which type of token it is; you can't go with a shorter match even
   if it would save a failing parse.  i.e. `doublev ` should be parsed as the identifier
   `doublev` with a space left over, not as the keyword `do` or `double`.
   
 - deciding between keyword/identifier/null/boolean part 2:  instead of having separate rules for
   each of these, the correct strategy seems to be to get the longest match of the identifier 
   pattern, then classify it into the correct category.  I have not yet found any drawbacks to 
   doing this.
   
 - `\n`, `\r`, and `\r\n` are the line terminators, apparently, and `\f` is a whitespace character

 - remember to use `InputCharacter`, not `item`, to consume a single *Java* character, since Java
   has the stream undergo unicode something-or-other.
 
 - `0` can only be parsed as a decimal literal if there's no underscores or digits following it
   (I think).  Thus, `0_0` should be parsed as an octal literal, not as the decimal literal `0` 
   with `_0` remaining.  I think this can be implemented simply by trying the octal rule before
   the decimal one, and not making any changes to the decimal rule.  But since this version won't
   deal with octal literals, it will mis-parse them.
   
 - what about the tokens `@` and `...` (ellipsis)?  I think these weren't in the spec.
 
 - the order that operator matches are tried in matters ... want to parse `==` as double-equals,
   not as two separate equals tokens

   
Parsing/AST:

 - it seems like there's a sub-unit missing in the types definition.  Shouldn't 
   `Identifier  TypeParameters(?)` be a reusable subunit?

 - 

