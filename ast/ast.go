package ast

type Node struct {
	Type      string
	Binary    bool // the binary feature is inherited from the parent
	Ignore    bool
	Existence bool
	SafeSpot  bool    // only terminal nodes can be safe spots
	Children  []*Node // exactly one of Children or
	Text      string  // Text or
	Bytes     []byte  // Bytes has to be filled
}

func (n *Node) MakeBinary() {
	if n.Binary {
		return // prevent endless recursion
	}
	n.Binary = true
	for _, child := range n.Children {
		child.MakeBinary()
	}
}
