package grammar

import (
	"errors"
	"maps"

	"github.com/flowdev/semtool/ast"

	"github.com/flowdev/comb"
	"github.com/flowdev/comb/cmb"
)

var allOptions = []string{"ignore", "existence", "safeSpot", "binary"}

type rule struct {
	name      string
	ignore    bool
	existence bool
	safeSpot  bool
	binary    bool
	rhs       comb.Parser[*ast.Node]
	children  map[string]comb.Parser[*ast.Node]
}

func (r rule) applyLHS() comb.Parser[*ast.Node] {
	return comb.NewParser("ruleLHS", func(state comb.State) (comb.State, *ast.Node, *comb.ParserError) {
		nState, node, err := r.rhs.Parse(state)
		if node == nil {
			return nState, node, err
		}
		node.Type = r.name
		if r.ignore {
			node.Ignore = true
		}
		if r.existence {
			node.Existence = true
		}
		if r.safeSpot {
			node.SafeSpot = true
		}
		if r.binary {
			node.MakeBinary()
		}
		return nState, node, err
	}, cmb.Forbidden())
}

type Parser struct {
	Grammar     comb.Parser[*ast.Node]
	Variable    comb.Parser[*ast.Node]
	Placeholder comb.Parser[*ast.Node]
	Escape      comb.Parser[*ast.Node]
}

func NewParser() comb.Parser[Parser] {
	// GRAMMAR         = Spacing Version Rule+ EOF
	return cmb.Map4(spacing(), version(), cmb.Many1(parseRule()), cmb.EOF(),
		func(_ string, _ int64, rules []rule, _ interface{}) (Parser, error) {
			allRules := make(map[string]comb.Parser[*ast.Node])
			p := Parser{}
			for _, r := range rules { // put all rules in a big map
				if _, ok := allRules[r.name]; ok {
					return p, errors.New("duplicate rule name: " + r.name)
				}
				allRules[r.name] = r.applyLHS()
			}
			for _, r := range rules { // give every rule its children
				for name := range r.children {
					if rr, ok := allRules[name]; ok {
						r.children[name] = rr
					} else {
						return p, errors.New("rule " + r.name + " references unknown rule " + name)
					}
				}
			}
			if g, ok := allRules["GRAMMAR"]; ok {
				p.Grammar = g
			}
			if v, ok := allRules["VARIABLE"]; ok {
				p.Variable = v
			}
			if ph, ok := allRules["PLACEHOLDER"]; ok {
				p.Placeholder = ph
			}
			if e, ok := allRules["ESCAPE"]; ok {
				p.Escape = e
			}
			return p, nil
		})
}

func version() comb.Parser[int64] {
	return cmb.Map2(cmb.Int64(false, 10), spacing(),
		func(i int64, _ string) (int64, error) {
			if i != 0 {
				return 0, errors.New("version must be 0")
			}
			return i, nil
		},
	)
}
func parseRule() comb.Parser[rule] {
	return cmb.Map4(parseName(), options(), assign(), rightHandSide(),
		func(name string, opts rule, _ rune, r rule) (rule, error) {
			r.name = name
			r.ignore = opts.ignore
			r.existence = opts.existence
			r.safeSpot = opts.safeSpot
			r.binary = opts.binary
			return r, nil
		})
}

func parseName() comb.Parser[string] {
	return cmb.Map2(cmb.AlphaMN(1, 1), cmb.Alphanumeric0(), func(first, rest string) (string, error) {
		return first + rest, nil
	})
}

func options() comb.Parser[rule] {
	op := cmb.Delimited(
		cmb.Map2(cmb.Char('('), spacing(), mapRuneSpacing),
		cmb.Separated0(
			cmb.Map2(cmb.OneOf(allOptions...), spacing(), mapStringSpacing),
			cmb.Map2(cmb.Char(','), spacing(), mapRuneSpacing),
			false,
		),
		cmb.Map2(cmb.Char(')'), spacing(), mapRuneSpacing),
	)

	return cmb.Map(op, func(options []string) (rule, error) {
		r := rule{}
		for _, option := range options {
			switch option {
			case "ignore":
				r.ignore = true
			case "existence":
				r.existence = true
			case "safeSpot":
				r.safeSpot = true
			case "binary":
				r.binary = true
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
		return r, nil
	})
}

func rightHandSide() comb.Parser[rule] {
	return cmb.Map(
		cmb.Separated1(sequence(), cmb.Map2(cmb.Char('/'), spacing(), mapRuneSpacing), false),
		func(seqs []rule) (rule, error) {
			r := rule{children: make(map[string]comb.Parser[*ast.Node])}
			ps := make([]comb.Parser[*ast.Node], len(seqs))
			for i, seq := range seqs {
				ps[i] = seq.rhs
				maps.Copy(r.children, seq.children)
			}
			r.rhs = cmb.FirstSuccessful(ps...)
			return r, nil
		},
	)
}

func sequence() comb.Parser[rule] {
	return nil
}

func assign() comb.Parser[rune] {
	return cmb.Map2(cmb.Char('='), spacing(), mapRuneSpacing)
}

func spacing() comb.Parser[string] {
	return cmb.Whitespace0()
}

func mapStringSpacing(s, _ string) (string, error) {
	return s, nil
}
func mapRuneSpacing(r rune, _ string) (rune, error) {
	return r, nil
}
