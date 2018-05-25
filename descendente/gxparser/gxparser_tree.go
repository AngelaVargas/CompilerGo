package gxparser

import (
	"fmt"
	"strings"
)

type Prog struct {
	c     string
	items []*Items
	depth int
}

type Items struct {
	types *Types
	funcs *Func
	depth int
}

type Types struct {
	c       string
	type_id string
	args    []string
	depth   int
}

type Func struct {
	c       string
	func_id string
	args    []string
	body    []*Body
	depth   int
}

type Body struct {
	typedecl *TypeDecl
	funcall  *Call
	decl     *Decl
	iter     *Iter
	cond     *Cond
	depth    int
}

type TypeDecl struct {
	c       string
	type_id string
	id      string
	depth   int
}

type Call struct {
	c       string
	func_id string
	params  []*Expr
	depth   int
}

type Decl struct {
	c      string
	var_id string
	field  []string
	expr   *Expr
	depth  int
}

type Iter struct {
	c          string
	asig       *Asig
	expr       *Expr
	secondexpr *Expr
	iter_body  []*Body
	depth      int
}

type Asig struct {
	c    string
	id   string
	expr *Expr
}

type Cond struct {
	c         string
	expr      *Expr
	if_body   []*Body
	else_body []*Body
	depth     int
}

type Expr struct {
	op     string
	left   []*Expr
	right  []*Expr
	leaf   string
	isLeaf bool
}

func NewProg() (node *Prog) {
	return &Prog{items: nil}
}

func NewItems() (node *Items) {
	return &Items{types: nil, funcs: nil}
}

func NewTypes() (node *Types) {
	return &Types{args: nil}
}

func NewFunc() (node *Func) {
	return &Func{args: nil, body: nil}
}

func NewBody() (node *Body) {
	return &Body{typedecl: nil, funcall: nil, decl: nil, iter: nil, cond: nil}
}

func NewTypeDecl() (node *TypeDecl) {
	return &TypeDecl{}
}

func NewCall() (node *Call) {
	return &Call{}
}

func NewDecl() (node *Decl) {
	return &Decl{field: nil, expr: nil}
}

func NewIter() (node *Iter) {
	return &Iter{asig: nil, expr: nil, secondexpr: nil, iter_body: nil}
}

func NewAsig() (node *Asig) {
	return &Asig{expr: nil}
}

func NewCond() (node *Cond) {
	return &Cond{expr: nil, if_body: nil, else_body: nil}
}

func NewExpr() (node *Expr) {
	return &Expr{left: nil, right: nil, isLeaf: false}
}

func (t *Prog) PrintTree() (s string) {
	tabs := strings.Repeat("  ", t.depth)
	s += fmt.Sprintf("%s %s: \n", tabs, t.c)
	s += fmt.Sprintf("%s Items: \n", tabs)
	for _, i := range t.items {
		s += i.PrintTreeItems()
	}
	return s
}

func (t *Items) PrintTreeItems() (s string) {
	tabs := strings.Repeat("  ", t.depth)
	s += fmt.Sprintf("%s", tabs)
	if t.types != nil {
		s += t.types.PrintTreeTypes()
	}
	if t.funcs != nil {
		s += t.funcs.PrintTreeFunc()
	}
	return s
}

func (t *Types) PrintTreeTypes() (s string) {
	tabs := strings.Repeat("  ", t.depth)
	s += fmt.Sprintf("%s %s %s (", tabs, t.c, t.type_id)
	for _, i := range t.args {
		s += fmt.Sprintf("%s ", i)
	}
	s += fmt.Sprintf(")\n")
	return s
}

func (t *Func) PrintTreeFunc() (s string) {
	tabs := strings.Repeat("  ", t.depth)
	s += fmt.Sprintf("%s %s %s (", tabs, t.c, t.func_id)
	for _, i := range t.args {
		s += fmt.Sprintf("%s ", i)
	}
	s += fmt.Sprintf("):\n")
	s += fmt.Sprintf("%s Body:\n", tabs)
	for _, i := range t.body {
		s += i.PrintTreeBody()
	}
	return s
}

func (t *Body) PrintTreeBody() (s string) {
	if t.typedecl != nil {
		s += t.typedecl.PrintTreeTypeDecl()
	}
	if t.funcall != nil {
		s += t.funcall.PrintTreeCall()
	}
	if t.decl != nil {
		s += t.decl.PrintTreeDecl()
	}
	if t.iter != nil {
		s += t.iter.PrintTreeIter()
	}
	if t.cond != nil {
		s += t.cond.PrintTreeCond()
	}
	return s
}

func (t *TypeDecl) PrintTreeTypeDecl() (s string) {
	tabs := strings.Repeat("  ", t.depth)
	s += fmt.Sprintf("%s %s: %s %s", tabs, t.c, t.type_id, t.id)
	s += fmt.Sprintf("\n")
	return s
}

func (t *Call) PrintTreeCall() (s string) {
	tabs := strings.Repeat("  ", t.depth)
	s += fmt.Sprintf("%s %s: %s (", tabs, t.c, t.func_id)
	for _, i := range t.params {
		s += i.PrintTreeExpr()
	}
	s += fmt.Sprintf(")\n")
	return s
}

func (t *Decl) PrintTreeDecl() (s string) {
	tabs := strings.Repeat("  ", t.depth)
	s += fmt.Sprintf("%s %s: %s", tabs, t.c, t.var_id)
	for _, i := range t.field {
		s += fmt.Sprintf(".%s ", i)
	}
	s += fmt.Sprintf("= ")
	if t.expr != nil {
		s += t.expr.PrintTreeExpr()
	}
	s += fmt.Sprintf("\n")
	return s
}

func (t *Iter) PrintTreeIter() (s string) {
	tabs := strings.Repeat("  ", t.depth)
	s += fmt.Sprintf("%s %s: ( ", tabs, t.c)
	if t.asig != nil {
		s += t.asig.PrintTreeAsig()
	}
	s += fmt.Sprintf(", ")
	if t.expr != nil {
		s += t.expr.PrintTreeExpr()
	}
	s += fmt.Sprintf(", ")
	if t.secondexpr != nil {
		s += t.secondexpr.PrintTreeExpr()
	}
	s += fmt.Sprintf(")\n")
	s += fmt.Sprintf("%s Iter_body:\n", tabs)
	for _, i := range t.iter_body {
		s += i.PrintTreeBody()
	}
	return s
}

func (t *Asig) PrintTreeAsig() (s string) {
	s += fmt.Sprintf("%s: %s:=", t.c, t.id)
	if t.expr != nil {
		s += t.expr.PrintTreeExpr()
	}
	return s
}

func (t *Cond) PrintTreeCond() (s string) {
	tabs := strings.Repeat("  ", t.depth)
	s += fmt.Sprintf("%s %s: (", tabs, t.c)
	if t.expr != nil {
		s += t.expr.PrintTreeExpr()
	}
	s += fmt.Sprintf(")\n")
	s += fmt.Sprintf("%s If_body:\n", tabs)
	for _, i := range t.if_body {
		s += i.PrintTreeBody()
	}
	s += fmt.Sprintf("%s Else_body:\n", tabs)
	for _, i := range t.else_body {
		s += i.PrintTreeBody()
	}
	return s
}

func (t *Expr) PrintTreeExpr() (s string) {
	if t.isLeaf == true {
		s += fmt.Sprintf(" %s ", t.leaf)
		return s
	}
	for _, i := range t.left {
		s += i.PrintTreeExpr()
	}
	if t.op != "" {
		s += fmt.Sprintf("%s", t.op)
	}
	for _, i := range t.right {
		s += i.PrintTreeExpr()
	}
	return s
}
