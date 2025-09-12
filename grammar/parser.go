package grammar

import (
	"errors"
	"fmt"
	"maps"
	"slices"
	"strings"

	"github.com/flowdev/semtool/ast"

	"github.com/flowdev/comb"
	"github.com/flowdev/comb/cmb"
)

var allOptions = []string{"ignore", "existence", "safeSpot", "binary", "text"}
var semtoolRules = []string{"VARIABLE", "PLACEHOLDER", "ESCAPE"}
var predefinedRules = map[string]rule{
	"VARIABLE":    {name: "VARIABLE", rhs: variableRuleRHS{}, safeSpot: true},
	"PLACEHOLDER": {name: "PLACEHOLDER", rhs: placeholderRuleRHS{}},
	"ESCAPE":      {name: "ESCAPE", rhs: escapeRuleRHS{}},
	"EOF":         {name: "EOF", rhs: eofRuleRHS{}, ignore: true},
	"EOL":         {name: "EOL", rhs: eolRuleRHS{}, existence: true},
	"FLOAT":       {name: "FLOAT", rhs: floatRuleRHS{}, safeSpot: true},
	"INTEGER":     {name: "INTEGER", rhs: integerRuleRHS{}, safeSpot: true},
	"SPACE":       {name: "SPACE", rhs: spaceRuleRHS{}, ignore: true},
	"MUST_SPACE":  {name: "MUST_SPACE", rhs: mspaceRuleRHS{}, existence: true},
	"NAME":        {name: "NAME", rhs: nameRuleRHS{}},
}

//- `EOF` parses the end of the input.
//- `EOL` parses the end of a line (`'\r\n', `'\n'` or `'\r``).
//- `FLOAT` parses a floating point number (without a sign).
//- `INTEGER` parses an integer number (without a sign).
//- `SPACE` parses any amount of Unicode whitespace (including none).
//- `MUST_SPACE` parses Unicode whitespace (at least one character).
//- `NAME` parses a name (a Unicode letter followed by zero or more Unicode letters, Unicode digits or underscores).

type Parser struct {
	allRules map[string]rule
	grammar  rule
}

func (p Parser) Parser(withSemtoolRules bool) comb.Parser[*ast.Node] {
	if p.grammar.inputType == ast.InputUnknown {
		p.grammar.inputType = ast.InputText // default to text
	}
	return p.grammar.parser(withSemtoolRules, p.grammar.inputType, p.allRules)
}

func NewGrammarParser() comb.Parser[Parser] {
	// GRAMMAR = Spacing Version Rule+ EOF ;;
	return cmb.Map4(spacing(), version(), cmb.Many1(parseRule()), cmb.EOF(),
		func(_ string, _ int64, rules []rule, _ interface{}) (Parser, error) {
			allRules := make(map[string]rule)
			maps.Copy(allRules, predefinedRules)
			p := Parser{}
			for _, r := range rules { // put all rules in a big map
				if _, ok := allRules[r.name]; ok {
					return p, errors.New("duplicate rule name: " + r.name)
				}
				allRules[r.name] = r
			}
			p.allRules = allRules
			if r, ok := allRules["GRAMMAR"]; ok {
				p.grammar = r
			}
			return p, nil
		})
}

func version() comb.Parser[int64] {
	// Version<safeSpot> = 'version' MustSpacing INTEGER Spacing ;;
	return cmb.Map4(
		comb.SafeSpot(cmb.String("version")),
		mustSpacing(),
		comb.SafeSpot(cmb.Int64(false, 10)),
		spacing(),
		func(_ string, _ string, i int64, _ string) (int64, error) {
			if i != 0 {
				return 0, errors.New("version must be 0")
			}
			return i, nil
		},
	)
}

type rule struct {
	name      string
	ignore    bool
	existence bool
	safeSpot  bool
	inputType ast.InputType
	//rhs       ruleExpression
	rhs parserer
}

func (r rule) parser(withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule) comb.Parser[*ast.Node] {
	it := inputType
	if r.inputType != ast.InputUnknown {
		it = r.inputType
	}
	return cmb.Map(r.rhs.parser(withSemtoolRules, it, allRules), func(node *ast.Node) (*ast.Node, error) {
		if node == nil {
			return node, fmt.Errorf("right hand side of rule %q produced no AST node", r.name)
		}
		node.Type = r.name
		if r.ignore {
			node.Ignore = true
		}
		if r.existence {
			node.Existence = true
		}
		return node, nil
	})
}

func parseRule() comb.Parser[rule] {
	// Rule	= Identifier Options? Assign Expression RuleEnd ;;
	return cmb.Map4(parseName(), cmb.Optional(options()), assign(), parseRuleExpression(),
		func(name string, opts rule, _ rune, re ruleExpression) (rule, error) {
			opts.name = name
			opts.rhs = re
			return opts, nil
		})
}

func options() comb.Parser[rule] {
	// Options<ignore>     = Begin AstOption ,+ Comma End ;;
	// AstOption<safeSpot> = ('ignore' / 'existence' / 'safeSpot' / 'binary' / 'text') Spacing ;;
	op := cmb.Delimited(
		cmb.Map2(cmb.Char('<'), spacing(), mapValueSpacing),
		cmb.Separated1(
			comb.SafeSpot(cmb.OneOf(allOptions...)),
			cmb.Map3(spacing(), cmb.Char(','), spacing(), mapSpacingValueSpacing),
			false,
		),
		cmb.Map3(spacing(), cmb.Char('>'), spacing(), mapSpacingValueSpacing),
	)

	return cmb.Map(op, func(options []string) (rule, error) {
		r := rule{}
		text := false
		binary := false
		for _, option := range options {
			switch option {
			case "ignore":
				r.ignore = true
			case "existence":
				r.existence = true
			case "safeSpot":
				r.safeSpot = true
			case "binary":
				binary = true
			case "text":
				text = true
			}
		}

		if r.ignore && r.existence {
			return r, errors.New("options 'ignore' and 'existence' can't be set together")
		}
		if r.ignore && r.safeSpot {
			return r, errors.New("options 'ignore' and 'safeSpot' can't be set together")
		}
		if r.existence && r.safeSpot {
			return r, errors.New("options 'existence' and 'safeSpot' can't be set together")
		}
		if binary && text {
			return r, errors.New("options 'binary' and 'text' can't be set together")
		}
		if text {
			r.inputType = ast.InputText
		} else if binary {
			r.inputType = ast.InputBinary
		}
		return r, nil
	})
}

func assign() comb.Parser[rune] {
	// Assign	= '=' Spacing ;;
	return cmb.Map2(cmb.Char('='), spacing(), mapValueSpacing)
}

type ruleExpression struct {
	sequences []sequence
}

func (re ruleExpression) parser(withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule) comb.Parser[*ast.Node] {
	parsers := make([]comb.Parser[*ast.Node], 0, len(re.sequences))
	for _, seq := range re.sequences {
		if p := seq.parser(withSemtoolRules, inputType, allRules); p != nil {
			parsers = append(parsers, p)
		}
	}
	if len(parsers) == 0 {
		return nil
	}
	if len(parsers) == 1 {
		return parsers[0]
	}
	return cmb.FirstSuccessful(parsers...)
}

func parseRuleExpression() comb.Parser[ruleExpression] {
	// Expression = Sequence ,+ Slash ;;
	return cmb.Map(
		cmb.Separated1(parseSequence(), cmb.Map2(cmb.Char('/'), spacing(), mapValueSpacing), false),
		func(seqs []sequence) (ruleExpression, error) {
			return ruleExpression{sequences: seqs}, nil
		},
	)
}

type parserer interface {
	parser(withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule) comb.Parser[*ast.Node]
}

type sequence struct {
	subSequences []subsequence
}

func (s sequence) parser(withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule) comb.Parser[*ast.Node] {
	parsers := make([]comb.Parser[*ast.Node], len(s.subSequences))
	for i, subSeq := range s.subSequences {
		p := subSeq.parser(withSemtoolRules, inputType, allRules)
		if p == nil { // withSemtoolRules == true and found semtool rule
			return nil
		}
		parsers[i] = p
	}
	return cmb.Map(cmb.Sequence(parsers...), mapNodes)
}

func parseSequence() comb.Parser[sequence] {
	// Sequence	    = Subsequence+ ;;
	return cmb.Map(cmb.Many1(parseSubsequence()), func(subs []subsequence) (sequence, error) {
		return sequence{subSequences: subs}, nil
	})
}

type subsequence struct {
	prefixOp   string
	primary1   primary
	postfixOp1 string
	infixOp    string
	primary2   primary
	postfixOp2 string
}

func (sub subsequence) parser(withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule) comb.Parser[*ast.Node] {
	if sub.infixOp != "" {
		p1 := applyPostfixOp(sub.postfixOp1, sub.primary1.parser(withSemtoolRules, inputType, allRules))
		p2 := applyPostfixOp(sub.postfixOp2, sub.primary2.parser(withSemtoolRules, inputType, allRules))
		switch sub.infixOp {
		// InfixOp = (CommaStar / CommaPlus / SemiStar / SemiPlus) Spacing ;;
		case ",*":
			return cmb.Map(cmb.Separated0(p1, p2, false), mapNodes)
		case ",+":
			return cmb.Map(cmb.Separated1(p1, p2, false), mapNodes)
		case ";*":
			return cmb.Map(cmb.Separated0(p1, p2, true), mapNodes)
		case ";+":
			return cmb.Map(cmb.Separated1(p1, p2, true), mapNodes)
		}
	}
	p := applyPostfixOp(sub.postfixOp1, sub.primary1.parser(withSemtoolRules, inputType, allRules))
	switch sub.prefixOp {
	// PrefixOp = (And / Not / RightArrow) Spacing ;;
	case "&":
		return cmb.Peek(p)
	case "!":
		return cmb.Map(cmb.Not(p), func(b bool) (*ast.Node, error) {
			if b {
				return nil, nil
			}
			return nil, errors.New("expected not " + p.Expected())
		})
	case "->":
		if inputType != ast.InputBinary {
			return cmb.Map(cmb.StringUntil(p), func(s string) (*ast.Node, error) {
				return &ast.Node{OutputType: ast.NodeText, Text: s}, nil
			})
		}
		return cmb.Map(cmb.BytesUntil(p), func(bs []byte) (*ast.Node, error) {
			return &ast.Node{OutputType: ast.NodeBinary, Bytes: bs}, nil
		})
	default:
		return p
	}
}

func applyPostfixOp(op string, p comb.Parser[*ast.Node]) comb.Parser[*ast.Node] {
	switch op {
	// PostfixOp = (Question / Star / Plus) Spacing ;;
	case "?":
		return cmb.Optional(p)
	case "*":
		return cmb.Map(cmb.Many0(p), mapNodes)
	case "+":
		return cmb.Map(cmb.Many1(p), mapNodes)
	default:
		return p
	}
}

func parseSubsequence() comb.Parser[subsequence] {
	// Subsequence	= Primary PostfixOp? InfixOp Primary PostfixOp?
	//               / PrefixOp? Primary PostfixOp? ;;
	return cmb.FirstSuccessful(
		cmb.Map5(
			parsePrimary(),
			cmb.Optional(cmb.Map2(cmb.OneOf("?", "*", "+"), spacing(), mapValueSpacing)),
			cmb.Map2(cmb.OneOf(",*", ",+", ";*", ";+"), spacing(), mapValueSpacing),
			parsePrimary(),
			cmb.Optional(cmb.Map2(cmb.OneOf("?", "*", "+"), spacing(), mapValueSpacing)),
			func(primary1 primary, postOp1 string, infixOp string, primary2 primary, postOp2 string) (subsequence, error) {
				return subsequence{
					primary1:   primary1,
					postfixOp1: postOp1,
					infixOp:    infixOp,
					primary2:   primary2,
					postfixOp2: postOp2,
				}, nil
			},
		),
		cmb.Map3(
			cmb.Optional(cmb.Map2(cmb.OneOf("&", "!", "->"), spacing(), mapValueSpacing)),
			parsePrimary(),
			cmb.Optional(cmb.Map2(cmb.OneOf("?", "*", "+"), spacing(), mapValueSpacing)),
			func(preOp string, prim primary, postOp string) (subsequence, error) {
				return subsequence{
					prefixOp:   preOp,
					primary1:   prim,
					postfixOp1: postOp,
				}, nil
			},
		),
	)
}

type primary struct {
	parserer parserer
}

func (p primary) parser(withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule) comb.Parser[*ast.Node] {
	return p.parserer.parser(withSemtoolRules, inputType, allRules)
}

func parsePrimary() comb.Parser[primary] {
	// Primary	= Identifier
	//           / Open Expression Close
	//           / Literal
	//           / Class
	//           / Dot ;;
	return cmb.FirstSuccessful(
		cmb.Map2(parseName(), spacing(), func(name string, _ string) (primary, error) { // Identifier
			return primary{parserer: identifier(name)}, nil
		}),
		cmb.Map( // Open Expression Close
			cmb.Delimited(
				cmb.Map2(cmb.Char('('), spacing(), mapValueSpacing),
				parseRuleExpression(),
				cmb.Map2(cmb.Char(')'), spacing(), mapValueSpacing),
			),
			func(re ruleExpression) (primary, error) {
				return primary{parserer: re}, nil
			},
		),
		cmb.Map( // Literal
			cmb.FirstSuccessful(
				cmb.Delimited(
					cmb.Map2(cmb.Char('\''), spacing(), mapValueSpacing),
					cmb.Many1(cmb.QuotedChar("single quoted character", "'", "'")),
					cmb.Map2(cmb.Char('\''), spacing(), mapValueSpacing),
				),
				cmb.Delimited(
					cmb.Map2(cmb.Char('"'), spacing(), mapValueSpacing),
					cmb.Many1(cmb.QuotedChar("double quoted character", "\"", "\"")),
					cmb.Map2(cmb.Char('"'), spacing(), mapValueSpacing),
				),
			),
			func(runes []rune) (primary, error) {
				return primary{parserer: literal{token: string(runes)}}, nil
			},
		),
		cmb.Map( // Class
			cmb.Delimited(
				// Class	 = '[' (!']' Range)+ ']' Spacing ;;
				cmb.Char('['),
				cmb.Many1(cmb.FirstSuccessful(
					// Range	 = RangeChar '-' RangeChar / RangeChar ;;
					cmb.Map3(
						cmb.QuotedChar("range character", "[]-", "[]-"),
						cmb.Char('-'),
						cmb.QuotedChar("range character", "[]-", "[]-"),
						func(r1 rune, _ rune, r2 rune) (class, error) {
							return class{ranges: [][]rune{{r1, r2}}}, nil
						}),
					// RangeChar = '\\[' / '\\]' / '\\-' / !('[' / ']' / '-') Char ;;
					cmb.Map(cmb.QuotedChar("range character", "[]-", "[]-"),
						func(r rune) (class, error) {
							return class{singleRangeChars: []rune{r}}, nil
						}),
				)),
				cmb.Map2(cmb.Char(']'), spacing(), mapValueSpacing),
			),
			func(classes []class) (primary, error) {
				ret := class{singleRangeChars: make([]rune, 0, 16), ranges: make([][]rune, 0, 8)}
				for _, cl := range classes {
					if len(cl.singleRangeChars) > 0 {
						ret.singleRangeChars = append(ret.singleRangeChars, cl.singleRangeChars[0])
					} else {
						ret.ranges = append(ret.ranges, cl.ranges[0])
					}
				}
				return primary{parserer: ret}, nil
			},
		),
		cmb.Map2(cmb.Char('.'), spacing(), func(r rune, _ string) (primary, error) { // Dot
			return primary{parserer: dot{}}, nil
		}),
	)
}

type identifier string

func (id identifier) parser(withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule) comb.Parser[*ast.Node] {
	if !withSemtoolRules && slices.Contains(semtoolRules, string(id)) {
		return nil
	}
	if r, ok := allRules[string(id)]; ok {
		return r.parser(withSemtoolRules, inputType, allRules)
	}
	return nil
}

type dot struct{}

func (d dot) parser(withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule) comb.Parser[*ast.Node] {
	if inputType == ast.InputBinary {
		return cmb.Map(cmb.AnyByte(), func(b byte) (*ast.Node, error) {
			return &ast.Node{OutputType: ast.NodeBinary, Bytes: []byte{b}}, nil
		})
	}
	return cmb.Map(cmb.AnyChar(), func(r rune) (*ast.Node, error) {
		return &ast.Node{OutputType: ast.NodeText, Text: string(r)}, nil
	})
}

type literal struct {
	token string
}

func (l literal) parser(withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule) comb.Parser[*ast.Node] {
	return cmb.Map(cmb.String(l.token), func(s string) (*ast.Node, error) {
		return &ast.Node{OutputType: ast.NodeText, Text: s}, nil
	})
}

type class struct {
	singleRangeChars []rune
	ranges           [][]rune
}

func (c class) parser(withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule) comb.Parser[*ast.Node] {
	return cmb.Map(cmb.Many1(cmb.CharClassChar("range character", c.singleRangeChars, c.ranges)), func(runes []rune) (*ast.Node, error) {
		return &ast.Node{OutputType: ast.NodeText, Text: string(runes)}, nil
	})
}

func spacing() comb.Parser[string] {
	// Spacing<ignore>	= SpaceComment* ;;
	// SpaceComment	    = (MUST_SPACE / Comment) ;;
	return cmb.Map(cmb.Many0(cmb.FirstSuccessful(cmb.Whitespace1(), comment())),
		func(spacings []string) (string, error) {
			return strings.Join(spacings, ""), nil
		})
}

func mustSpacing() comb.Parser[string] {
	// MustSpacing<existence>  = SpaceComment+ ;;
	// SpaceComment	           = (MUST_SPACE / Comment) ;;
	return cmb.Map(cmb.Many1(cmb.FirstSuccessful(cmb.Whitespace1(), comment())),
		func(spacings []string) (string, error) {
			return strings.Join(spacings, ""), nil
		})
}

func comment() comb.Parser[string] {
	// Comment	= '#' -> EOL ;;
	return cmb.Map2(cmb.Char('#'), cmb.StringUntil(eol()),
		func(r rune, s string) (string, error) {
			return string(r) + s, nil
		})
}

//
// Predefined rules
//

type variableRuleRHS struct{}

func (vr variableRuleRHS) parser(
	withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule,
) comb.Parser[*ast.Node] {
	return cmb.Map2(cmb.Char('$'), parseName(), func(r rune, name string) (*ast.Node, error) {
		return &ast.Node{OutputType: ast.NodeText, Variable: true, Text: name}, nil
	})
}

type placeholderRuleRHS struct{}

func (pr placeholderRuleRHS) parser(
	withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule,
) comb.Parser[*ast.Node] {
	return cmb.Map(cmb.Char('_'), func(r rune) (*ast.Node, error) {
		return &ast.Node{OutputType: ast.NodeText, Variable: true, Text: ""}, nil
	})
}

type escapeRuleRHS struct{}

func (er escapeRuleRHS) parser(
	withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule,
) comb.Parser[*ast.Node] {
	return cmb.Map(cmb.Char('\\'), func(r rune) (*ast.Node, error) {
		return &ast.Node{OutputType: ast.NodeText, Escape: true}, nil
	})
}

type eofRuleRHS struct{}

func (eofr eofRuleRHS) parser(
	withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule,
) comb.Parser[*ast.Node] {
	return cmb.Map(cmb.EOF(), func(_ interface{}) (*ast.Node, error) {
		return &ast.Node{OutputType: ast.NodeType(inputType)}, nil
	})
}

type eolRuleRHS struct{}

func (eolr eolRuleRHS) parser(
	withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule,
) comb.Parser[*ast.Node] {
	return cmb.Map(cmb.OneOf("\r\n", "\n", "\r"), func(eol string) (*ast.Node, error) {
		return &ast.Node{OutputType: ast.NodeText, Text: eol}, nil
	})
}

type floatRuleRHS struct{}

func (floatr floatRuleRHS) parser(
	withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule,
) comb.Parser[*ast.Node] {
	return cmb.Map(cmb.Float(false, 10, false, true), func(float string) (*ast.Node, error) {
		return &ast.Node{OutputType: ast.NodeText, Text: float}, nil
	})
}

type integerRuleRHS struct{}

func (integerr integerRuleRHS) parser(
	withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule,
) comb.Parser[*ast.Node] {
	return cmb.Map(cmb.Integer(false, 10, false), func(integer string) (*ast.Node, error) {
		return &ast.Node{OutputType: ast.NodeText, Text: integer}, nil
	})
}

type spaceRuleRHS struct{}

func (spacer spaceRuleRHS) parser(
	withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule,
) comb.Parser[*ast.Node] {
	return cmb.Map(cmb.Whitespace0(), func(space string) (*ast.Node, error) {
		return &ast.Node{OutputType: ast.NodeText, Text: space}, nil
	})
}

type mspaceRuleRHS struct{}

func (mspacer mspaceRuleRHS) parser(
	withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule,
) comb.Parser[*ast.Node] {
	return cmb.Map(cmb.Whitespace1(), func(mspace string) (*ast.Node, error) {
		return &ast.Node{OutputType: ast.NodeText, Text: mspace}, nil
	})
}

type nameRuleRHS struct{}

func (nr nameRuleRHS) parser(
	withSemtoolRules bool, inputType ast.InputType, allRules map[string]rule,
) comb.Parser[*ast.Node] {
	return cmb.Map(parseName(), func(name string) (*ast.Node, error) {
		return &ast.Node{OutputType: ast.NodeText, Text: name}, nil
	})
}

//
// Predefined parsers
//

func parseName() comb.Parser[string] {
	return cmb.Map2(cmb.Alpha1(), cmb.Alphanumeric0(), func(first, rest string) (string, error) {
		return first + rest, nil
	})
}

func eol() comb.Parser[string] {
	return cmb.OneOf("\r\n", "\n", "\r")
}

//
// Predefined mappers
//

func mapValueSpacing[T any](v T, _ string) (T, error) {
	return v, nil
}
func mapSpacingValueSpacing[T any](_ string, v T, _ string) (T, error) {
	return v, nil
}

func mapNodes(nodes []*ast.Node) (*ast.Node, error) {
	j := 0
	for _, node := range nodes {
		if node != nil {
			nodes[j] = node
			j++
		}
	}
	nodes = nodes[:j]

	if len(nodes) == 0 {
		return nil, nil
	}
	if len(nodes) == 1 {
		return nodes[0], nil
	}
	return &ast.Node{OutputType: ast.NodeParent, Children: nodes}, nil
}
