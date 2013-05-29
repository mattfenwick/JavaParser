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
   
 - the spec uses `HexDigit` in two different places:  UnicodeEscapes (i.e. `\u00af`) and hexadecimal
   numerals (i.e. 0x12a3).  But they're different:  the first accepts only raw hex digits, whereas
   the second accepts raw *and* unicode-escaped hex digits.
   
 - hexadecimal floating point numbers with exponents:  the base is in hexadecimal, the exponent is
   written in decimal but is a binary exponent.  Examples:
     
        0x3.23p2; // 12.546875 in decimal, because it's `3.23 in base 16, * 2^2`, not 10^2 or 16^2,
                  //   and so it's `(3 + 2. / 16 + 3. / 64) * 4`
                  
        double d1 = 0x3.23pa2; // invalid because it uses a hex digit in the exponent
 
 - hex floating point exponents are introduced by p or P, instead of e or E as in decimal floating
   point literals.  Presumably this is because e and E are valid hex digits.

   
Parsing/AST:

 - it seems like there's a sub-unit missing in the types definition.  Shouldn't 
   `Identifier  TypeParameters(?)` be a reusable subunit?

 - oopsie-doopsie, changed grammar to only accept identifiers in type parameters, instead of full
   types.  This was just done for simplicity.  I plan to change it back later.

 - How is `A<B<C>>` tokenized, and how is it parsed?  The problem is that it should be tokenized 
   as `A, <, B, <, C, >>` according to the longest match rule, but then it would not be parsed as
   a correct type arguments thing.  But Eclipse parses it just fine.  The answer seems to be that
   it *is* tokenized the same way, but then the tokens can be reinterpreted during parsing, if
   necessary.  So tokens beginning with `>` will have the first `>` stripped off, then the shortened
   token is pushed back onto the token input stream.
   Question:  can this situation arise with type parameters, or is it only with type arguments?
   I don't think so, because type parameters can't be nested:  in `<A extends B<C>>`, the type
   arguments see the `>>` and deal with it before the type parameters do.
