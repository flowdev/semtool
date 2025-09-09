package ast

type InputType int

const (
	InputUnknown InputType = iota
	InputText
	InputBinary
)

type NodeType int

const (
	NodeParent = NodeType(InputUnknown)
	NodeText   = NodeType(InputText)
	NodeBinary = NodeType(InputBinary)
)

type Node struct {
	Type       string // name of the rule in the grammar
	OutputType NodeType
	Ignore     bool    // if true, this node should be ignored during comparison
	Existence  bool    // if true, only the existence of a node with this Type is checked during comparison
	Variable   bool    // if true, Text contains the name of the variable
	Escape     bool    // if true, the next node should be a variable or a placeholder
	Children   []*Node // exactly one of Children or
	Text       string  // Text or
	Bytes      []byte  // Bytes has to be filled
}
