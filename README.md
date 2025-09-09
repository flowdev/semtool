# semtool
Tool for semantic actions on any language or file format driven by PEG grammars.

## TODOs

1. Implement a simple parser for a PEG-like grammar with **comb**.
1. Get search file definition for SemGrep/OpenGrep.
1. Define a set of keywords/rules.
1. Implement parser for search files with **comb**.
1. Refine own PEG grammar parser.
1. Implement AST comparison.
1. Implement repacements with Go text templates.

## Own Syntax

- `=` is used for rule assignment.
- `/` is used for alternatives (`FirstSuccessful`).
- Separating multiple rules with space is used for sequences (`Sequence`).
- `*` and `+` are used for repetitions (`Many0` and `Many1`).
- `,*` and `,+` are used for lists (`Separated0` and `Separated1`) withOUT parsing a separator at the end.
- `;*` and `;+` are used for lists (`Separated0` and `Separated1`) WITH optional parsing of a separator at the end.
- `?` is used for optional parsers (`Optional`).
- `->` is used for parsing until another parser matches (`Until`).
- `.` is used for any character or byte in case of a binary parser (either `AnyChar` or `AnyByte`).
- `'` and `"` are used for string literals.
- `(` and `)` are used for grouping.
- `!` is used for negative lookahead.
- `&` is used for positive lookahead.
- `[` and `]` are used for character classes.
- In string literals ANSI escape sequences are supported and so are
  `\377`, `\xabcdef` and `\u00abcdef` for octal, hex and Unicode.
- Comments start with `#` and continue to the end of the line.
- Whitespace is ignored.

### Predifined rules

- `EOF` parses the end of the input.
- `EOL` parses the end of a line (`'\r\n', `'\n'` or `'\r``).
- `FLOAT` parses a floating point number (without a sign).
- `INTEGER` parses an integer number (without a sign).
- `SPACE` parses any amount of Unicode whitespace (including none).
- `MUST_SPACE` parses Unicode whitespace (at least one character).
- `NAME` parses a name (a Unicode letter followed by zero or more Unicode letters, Unicode digits or underscores).

### Predefined character classes

- `ALPHA` parses a Unicode letter.
- `DIGIT` parses a Unicode digit or number.
- `WORDO` is `ALPHA` and `DIGIT` plus `_` combined into a single class.

The names of these character classes are deliberately chosen to contain a double vowel.
So they aren't reasonable character classes
(for example `[WORD]` is a reasonable character class but `[WORDO]` isn't).

### Rules a user has to define

- `GRAMMAR` is the root rule of a grammar.
- `VARIABLE` parses a variable name (e.g. `'$' NAME`) in code snippets for searching or replacing.
  A variable can stand for any syntactically valid subtree of the current parse tree (AST).
- `PLACEHOLDER` parses a placeholder (e.g. `'_'` or `'$PLACEHOLDER'`) in code snippets for searching.
  Like a variable, a placeholder can stand for any syntactically valid subtree of the current parse tree (AST).
  But a placeholder can't be referenced later. So it can't be used in replacements.
- `BINARY` is more a variable than a rule. It can only be set to `true` or `false`.
  `false` is the default value. So it can be omitted.