package gxparser

import (
	"descendente/finlex"
	"descendente/gxsymbols"
	"errors"
	"fmt"
	"os"
	"strings"
)

type Parser struct {
	l      *finlex.Lexer
	stkEnv *gxsymbols.StkEnv
	depth  int
}

var DebugDesc bool = false

var types []string

func NewParser(l *finlex.Lexer) *Parser {
	env := gxsymbols.NewEnvStk()
	return &Parser{l, env, 0}
}

func (p *Parser) match(tT finlex.TokType) (t finlex.Token, e error, isMatch bool) {
	token, err := p.l.Peek()
	if err != nil {
		return t, err, false
	}
	if token.Type() != tT {
		return t, nil, false
	}
	t, e = p.l.Lex()
	return t, nil, true
}

func (p *Parser) matchtypes(tT []string) (t finlex.Token, e error, isMatch bool) {
	token, err := p.l.Peek()
	if err != nil {
		return t, err, false
	}
	tok := token.GetLexeme()
	if tok == ")" { //EMPTY
		return t, nil, false
	}
	if token.Type() != finlex.TokArgType {
		lex := token.GetLexeme()
		for _, i := range tT {
			if lex == i { // Es un tipo de la tabla de tipos, consumo token
				t, e = p.l.Lex()
				token.String()
				return t, nil, true
			}
		}
		err := errors.New("Error: Type not declared")
		return t, err, false // Error de tipo
	}
	t, e = p.l.Lex()
	token.String()
	return t, nil, true // Es un tipo: int, bool o Coord, consumo token
}

func (p *Parser) pushTrace(tag string) {
	if DebugDesc {
		tabs := strings.Repeat("\t", p.depth)
		fmt.Fprintf(os.Stderr, "%s %s \n", tabs, tag)
	}
	p.depth++
}

func (p *Parser) popTrace() {
	p.depth--
}

func (p *Parser) Parse() (Node *Prog, e error) {
	p.pushTrace("Parse")
	defer p.popTrace()
	parsernode, err := p.Prog()
	if err != nil {
		return parsernode, err
	}
	return parsernode, nil
}

//<PROG> ::= <ITEMS> EOF
func (p *Parser) Prog() (Node *Prog, e error) {
	p.pushTrace("Prog")
	defer p.popTrace()

	p.stkEnv.PushEnv() // Creo un nuevo ámbito
	defer p.stkEnv.PopEnv()

	prognode := NewProg() //Creo un nuevo nodo
	prognode.c = "Prog"
	prognode.depth = 0

	var itemsArray []*Items
	items, err := p.Items(itemsArray, prognode)

	if err != nil {
		return prognode, err
	}
	prognode.items = items

	_, err, isEof := p.match(finlex.TokEof)
	if err != nil || !isEof {
		return prognode, err
	}
	return prognode, nil //Ha ido todo bien
}

//<ITEMS> ::= 'type' 'record' type_id '(' <ARGS> ')' <ITEMS> |
//            'func' <FUNC> <ITEMS> |
//            empty
func (p *Parser) Items(itemsArray []*Items, n *Prog) (array []*Items, e error) {
	p.pushTrace("Items")
	defer p.popTrace()

	_, err, isType := p.match(finlex.TokDataType)
	if err != nil || !isType { //Si no es 'type', compruebo 'func'
		_, err, isFunc := p.match(finlex.TokFunc)
		if err != nil || !isFunc { //Tampoco es 'func' -> EMPTY
			return itemsArray, err
		} else { //Es 'func'
			funcnode := NewFunc()
			funcnode.c = "Func"
			funcnode.depth = n.depth + 1

			newfuncnode, err := p.Func(funcnode)
			if err != nil {
				return itemsArray, err
			}
			// Ha ido todo bien
			itemnode := NewItems()
			itemnode.depth = n.depth
			itemnode.funcs = newfuncnode

			itemsArray = append(itemsArray, itemnode)
		}
	} else { //Es 'type'
		typenode := NewTypes()
		typenode.c = "Type record"
		_, err, isRecord := p.match(finlex.TokRecord)
		if err != nil || !isRecord {
			return itemsArray, errors.New("Error: Missing word 'record'")
		}
		tok, err, isTypeId := p.match(finlex.TokId)
		if err != nil || !isTypeId {
			return itemsArray, errors.New("Error: Missing id of type record")
		}
		newtype := tok.GetLexeme()
		types = append(types, newtype) //Añado el nuevo tipo a la tabla de tipos
		typenode.type_id = newtype
		typenode.depth = n.depth + 1

		_, err, isBracket := p.match(finlex.TokType('('))
		if err != nil {
			return itemsArray, err
		}
		if !isBracket && err == nil {
			return itemsArray, errors.New("Error: Missing Opening bracket")
		}

		typeargsnode, err := p.Args()
		typenode.args = typeargsnode
		if err != nil {
			return itemsArray, err
		}
		_, err, isRightBracket := p.match(finlex.TokType(')'))
		if err != nil {
			return itemsArray, err
		}
		if !isRightBracket && err == nil {
			return itemsArray, errors.New("Error: Missing Closing bracket")
		}
		// Ha ido todo bien
		itemnode := NewItems()
		itemnode.depth = n.depth
		itemnode.types = typenode

		itemsArray = append(itemsArray, itemnode)
	}

	finalArray, err := p.Items(itemsArray, n)
	if err != nil {
		return finalArray, err
	}
	return finalArray, nil
}

//<FUNC> ::= func_id '(' <ARGS> ')' '{' <BODY> '}'
func (p *Parser) Func(funcnode *Func) (Node *Func, e error) {
	p.pushTrace("Func")
	defer p.popTrace()

	tok, err, isId := p.match(finlex.TokId)
	if err != nil {
		return funcnode, err
	}
	if !isId && err == nil { //Hay token pero no es id
		return funcnode, errors.New("Error: Bad type Id in function")
	}
	func_id := tok.GetLexeme() //Ha ido bien, obtendo func_id y lo guardo en la pila
	s := p.stkEnv.NewSymb(func_id, gxsymbols.Sfunc)
	if s == nil {
		return nil, errors.New("Stack error: Id already defined " + func_id)
	}

	funcnode.func_id = func_id
	_, err, isBracket := p.match(finlex.TokType('('))
	if err != nil {
		return funcnode, err
	}
	if !isBracket && err == nil { //Hay token pero no es (
		return funcnode, errors.New("Error: Missing Opening bracket")
	}

	p.stkEnv.PushEnv() //Creo un nuevo ámbito para los args y body de func
	defer p.stkEnv.PopEnv()

	argsnode, err := p.Args()
	funcnode.args = argsnode
	if err != nil {
		return funcnode, err
	}
	_, err, isRightBracket := p.match(finlex.TokType(')'))
	if err != nil { //EMPTY, no ha encontrado nada
		return funcnode, err
	}
	if !isRightBracket && err == nil { //Hay token pero no es )
		return funcnode, errors.New("Error: Missing Closing bracket")
	}
	_, err, isKey := p.match(finlex.TokType('{'))
	if err != nil { //EMPTY, no ha encontrado nada
		return funcnode, err
	}
	if !isKey && err == nil { //Hay token pero no es {
		return funcnode, errors.New("Error: Missing Opening Key")
	}

	var bodyArray []*Body
	bodynode, err := p.Body(funcnode.depth, bodyArray)

	if err != nil {
		return funcnode, err
	}
	funcnode.body = bodynode //Añado el array de Body, devuelto por Body, al nodo Func

	_, err, isRightKey := p.match(finlex.TokType('}'))
	if err != nil { //EMPTY, no ha encontrado nada
		return funcnode, err
	}
	if !isRightKey && err == nil { //Hay token pero no es }
		return funcnode, errors.New("Error: Missing Closing Key")
	}
	return funcnode, nil
}

//<ARGS> ::=  type_id id <OTHERARGS> |
//            empty
func (p *Parser) Args() (Node []string, e error) {
	p.pushTrace("Args")
	defer p.popTrace()

	var args []string
	argtype, err, isType := p.matchtypes(types)
	if err != nil || !isType { //no hay token -> EMPTY, o error de tipos
		return args, err
	}
	arg_type := argtype.GetLexeme()
	args = append(args, arg_type)
	tok, err, isId := p.match(finlex.TokId)
	if err != nil {
		return args, err
	}
	if !isId && err == nil {
		return args, errors.New("Error: Empty arg Id")
	}
	arg_id := tok.GetLexeme()
	args = append(args, arg_id)

	//Lo añado a la pila
	s := p.stkEnv.NewSymb(arg_id, gxsymbols.Svar)
	if s == nil {
		return nil, errors.New("Stack error: id already declared " + arg_id)
	}
	otherargsnode, err := p.Otherargs(args)
	if err != nil {
		return otherargsnode, err
	}
	args = otherargsnode
	return args, nil
}

//<OTHERARGS> ::= ',' type_id id <OTHERARGS> |
//               empty
func (p *Parser) Otherargs(args []string) (Node []string, e error) {
	p.pushTrace("OtherArgs")
	defer p.popTrace()

	_, err, isComa := p.match(finlex.TokType(','))
	if err != nil || !isComa { //no hay token o error de tipos -> EMPTY
		return args, err
	}
	argtype, err, isType := p.matchtypes(types)
	if err != nil {
		return args, err
	}
	if !isType && err == nil {
		return args, errors.New("Error: Bad arg type")
	}
	arg_type := argtype.GetLexeme()
	args = append(args, arg_type)

	argid, err, isId := p.match(finlex.TokId)
	if err != nil { //EMPTY, no ha encontrado nada
		return args, err
	}
	if !isId && err == nil {
		return args, errors.New("Error: Bad arg Id")
	}
	arg_id := argid.GetLexeme()
	args = append(args, arg_id)

	//Lo añado a la pila
	s := p.stkEnv.NewSymb(arg_id, gxsymbols.Svar)
	if s == nil {
		return nil, errors.New("Stack error: id already declared " + arg_id)
	}

	otherargs, err := p.Otherargs(args)
	if err != nil {
		return args, err
	}
	return otherargs, nil
}

//<BODY> ::=  type_id id ';' <BODY> |
//            func_id '(' <PARAMS> ')' ';' <BODY> |  --LLamada a función
//            var_id <FIELD> '=' <EXPR> ';' <BODY> |
//           'iter' '(' <DECL> ',' <EXPR> ',' <EXPR> ')' '{' <BODY> '}' <BODY> |
//           'if' '(' <EXPR> ')' '{' <BODY> '}' <ELSE> <BODY> |
//            empty
func (p *Parser) Body(depth int, bodyArray []*Body) (Node []*Body, e error) {
	p.pushTrace("Body")
	defer p.popTrace()

	tokTypeId, err, isTypeId := p.matchtypes(types) //Compruebo si es type_id
	switch {
	case err != nil || !isTypeId: //No es type_id, compruebo si es var_id o func_id en la tabla de símbolos
		id, err, isId := p.match(finlex.TokId)
		switch {
		case err != nil || !isId: //Tampoco es var_id o func_id, compruebo si es 'iter'
			_, err, isIter := p.match(finlex.TokIter)
			switch {
			case err != nil || !isIter: //No es 'iter', compruebo finalmente si es 'if':
				//'if' '(' <EXPR> ')' '{' <BODY> '}' <ELSE> <BODY>
				condnode := NewCond()
				condnode.c = "Cond"
				condnode.depth = depth + 1

				_, err, isIf := p.match(finlex.TokIf)
				if err != nil || !isIf {
					return bodyArray, err
				}
				_, err, isBracket := p.match(finlex.TokType('('))
				if err != nil { //EMPTY, no ha encontrado nada
					return bodyArray, err
				}
				if !isBracket && err == nil { //Hay token pero no es (
					return bodyArray, errors.New("Error: Missing Opening bracket")
				}
				exprNode := NewExpr()
				expr, erro := p.Expr(exprNode)
				if erro != nil {
					return bodyArray, erro
				}
				condnode.expr = expr

				_, err, isRightBracket := p.match(finlex.TokType(')'))
				if err != nil { //EMPTY, no ha encontrado nada
					return bodyArray, err
				}
				if !isRightBracket && err == nil { //Hay token pero no es )
					return bodyArray, errors.New("Error: Missing Closing bracket")
				}
				_, err, isKey := p.match(finlex.TokType('{'))
				if err != nil { //EMPTY, no ha encontrado nada
					return bodyArray, err
				}
				if !isKey && err == nil { //Hay token pero no es {
					return bodyArray, errors.New("Error: Missing Opening Key")
				}

				var ifArray []*Body
				ifArrayBody, err := p.Body(condnode.depth, ifArray)

				if err != nil {
					return bodyArray, err
				}
				condnode.if_body = ifArrayBody

				_, err, isRightKey := p.match(finlex.TokType('}'))
				if err != nil { //EMPTY, no ha encontrado nada
					return bodyArray, err
				}
				if !isRightKey && err == nil { //Hay token pero no es }
					return bodyArray, errors.New("Error: Missing Closing Key")
				}
				elsenode, err := p.Else(condnode)
				if err != nil {
					return bodyArray, err
				}
				if elsenode != nil {
					condnode.else_body = elsenode
				}
				//Ha ido todo bien
				bodycond := NewBody()
				bodycond.depth = depth + 2
				bodycond.cond = condnode

				bodyArray = append(bodyArray, bodycond)

			default: //Sí es iter, compruebo -> 'iter' '(' <DECL> ',' <EXPR> ',' <EXPR> ')' '{' <BODY> '}' <BODY>
				iternode := NewIter()
				iternode.c = "Iter"
				iternode.depth = depth + 1

				_, err, isBracket := p.match(finlex.TokType('('))
				if err != nil { //EMPTY, no ha encontrado nada
					return bodyArray, err
				}
				if !isBracket && err == nil { //Hay token pero no es (
					return bodyArray, errors.New("Error: Missing Opening bracket")
				}

				p.stkEnv.PushEnv() // Creo un nuevo ámbito para Decl y iter_body
				defer p.stkEnv.PopEnv()

				asig, err := p.Decl()
				if err != nil {
					return bodyArray, err
				}

				iternode.asig = asig

				_, err, isFirstComa := p.match(finlex.TokType(','))
				if err != nil { //EMPTY, no ha encontrado nada
					return bodyArray, err
				}
				if !isFirstComa && err == nil { //Hay token pero no es ,
					return bodyArray, errors.New("Error: Missing first coma ','")
				}
				exprNode := NewExpr()
				expr, er := p.Expr(exprNode)
				if er != nil {
					return bodyArray, er
				}
				iternode.expr = expr

				_, err, isSecondComa := p.match(finlex.TokType(','))
				if err != nil { //EMPTY, no ha encontrado nada
					return bodyArray, err
				}
				if !isSecondComa && err == nil { //Hay token pero no es ,
					return bodyArray, errors.New("Error: Missing second coma ','")
				}
				secondExprNode := NewExpr()
				secondexpr, e := p.Expr(secondExprNode)
				if e != nil {
					return bodyArray, e
				}
				iternode.secondexpr = secondexpr

				_, err, isRightBracket := p.match(finlex.TokType(')'))
				if err != nil { //EMPTY, no ha encontrado nada
					return bodyArray, err
				}
				if !isRightBracket && err == nil { //Hay token pero no es )
					return bodyArray, errors.New("Error: Missing Closing bracket")
				}
				_, err, isKey := p.match(finlex.TokType('{'))
				if err != nil { //EMPTY, no ha encontrado nada
					return bodyArray, err
				}
				if !isKey && err == nil { //Hay token pero no es {
					return bodyArray, errors.New("Error: Missing Opening Key")
				}

				var iterArray []*Body
				iterbody, err := p.Body(iternode.depth, iterArray)

				if err != nil {
					return bodyArray, err
				}

				iternode.iter_body = iterbody

				_, err, isRightKey := p.match(finlex.TokType('}'))
				if err != nil { //EMPTY, no ha encontrado nada
					return bodyArray, err
				}
				if !isRightKey && err == nil { //Hay token pero no es }
					return bodyArray, errors.New("Error: Missing Closing Key")
				}
				//Ha ido todo bien
				bodyiter := NewBody()
				bodyiter.depth = depth + 2
				bodyiter.iter = iternode

				bodyArray = append(bodyArray, bodyiter)
			}
		default: //Es var_id o func_id, comrpuebo cuál.
			_, err, isBracket := p.match(finlex.TokType('('))
			switch {
			case err != nil || !isBracket: // No es func_id, compruebo var_id:
				//var_id <FIELD> '=' <EXPR> ';' <BODY>
				declnode := NewDecl()
				declnode.c = "Decl"
				declnode.depth = depth + 1
				declnode.var_id = id.GetLexeme()

				//Compruebo si está en la pila
				s := p.stkEnv.GetSymb(declnode.var_id)
				if s == nil {
					return nil, errors.New("Stack error: Id not declared " + declnode.var_id)
				}

				var field []string

				fieldnode, err := p.Field(field)
				if err != nil {
					return bodyArray, err
				}

				declnode.field = fieldnode

				_, err, isEqual := p.match(finlex.TokType('='))
				if err != nil || !isEqual {
					return bodyArray, errors.New("Error: Missing '='")
				}
				exprNode := NewExpr()
				expr, e := p.Expr(exprNode)
				if e != nil {
					return bodyArray, e
				}

				declnode.expr = expr

				_, err, isFinalPoint := p.match(finlex.TokType(';'))
				if err != nil || !isFinalPoint {
					return bodyArray, errors.New("Error: Missing final ';'")
				}
				//Ha ido todo bien
				bodydecl := NewBody()
				bodydecl.depth = depth + 2
				bodydecl.decl = declnode

				bodyArray = append(bodyArray, bodydecl)
			default:
				//Es func_id, comrpuebo ->  func_id '(' <PARAMS> ')' ';' <BODY>
				callnode := NewCall()
				callnode.c = "Func call"
				callnode.depth = depth + 1
				callnode.func_id = id.GetLexeme()

				//Compruebo si está en la pila
				s := p.stkEnv.GetSymb(callnode.func_id)
				if s == nil {
					return nil, errors.New("Stack error: Id not declared " + callnode.func_id)
				}

				paramsnode, err := p.Params()
				callnode.params = paramsnode
				if err != nil {
					return bodyArray, err
				}
				_, err, isClosingBracket := p.match(finlex.TokType(')'))
				if err != nil || !isClosingBracket {
					return bodyArray, errors.New("Error: Missing closing bracket")
				}
				_, err, isFinalPoint := p.match(finlex.TokType(';'))
				if err != nil || !isFinalPoint {
					return bodyArray, errors.New("Error: Missing final ';'")
				}
				//Ha ido todo bien
				bodycall := NewBody()
				bodycall.depth = depth + 1
				bodycall.funcall = callnode

				bodyArray = append(bodyArray, bodycall)
			}
		}
	default:
		//Es type_id, lo compruebo -> type_id id ';' <BODY>
		typedecl := NewTypeDecl()
		typedecl.c = "TypeDecl"
		typedecl.depth = depth + 1
		typedecl.type_id = tokTypeId.GetLexeme()

		tokId, err, isId := p.match(finlex.TokId)
		if err != nil || !isId {
			return bodyArray, err
		}

		typedecl.id = tokId.GetLexeme()

		//Añado a la pila
		s := p.stkEnv.NewSymb(typedecl.id, gxsymbols.Svar)
		if s == nil {
			return nil, errors.New("Stack error: Id already defined " + typedecl.id)
		}

		_, err, isFinalPoint := p.match(finlex.TokType(';'))
		if err != nil || !isFinalPoint {
			return bodyArray, errors.New("Error: Missing final ';'")
		}
		//Ha ido todo bien
		bodytypedecl := NewBody()
		bodytypedecl.depth = depth + 1
		bodytypedecl.typedecl = typedecl

		bodyArray = append(bodyArray, bodytypedecl)
	}

	finalArray, err := p.Body(depth, bodyArray)
	if err != nil {
		return finalArray, err
	}
	return finalArray, nil
}

//<FIELD> ::= '.' record_field <FIELD> |
//            empty
func (p *Parser) Field(field []string) (Node []string, e error) {
	p.pushTrace("Field")
	defer p.popTrace()

	_, err, isPoint := p.match(finlex.TokType('.'))
	if err != nil || !isPoint {
		return field, err
	}
	tok, err, isRecordField := p.match(finlex.TokId)
	if err != nil || !isRecordField {
		return field, errors.New("Error: Missing id after '.'")
	}

	record_field := tok.GetLexeme()
	field = append(field, record_field)

	otherfield, err := p.Field(field)
	if err != nil {
		return otherfield, err
	}
	return otherfield, nil
}

//<ELSE> ::= 'else' '{' <BODY> '}' |
//           empty
func (p *Parser) Else(n *Cond) (Node []*Body, e error) {
	p.pushTrace("Else")
	defer p.popTrace()
	_, err, isElse := p.match(finlex.TokElse)
	if err != nil || !isElse {
		return nil, err
	}
	_, err, isKey := p.match(finlex.TokType('{'))
	if err != nil { //EMPTY, no ha encontrado nada
		return nil, err
	}
	if !isKey && err == nil { //Hay token pero no es {
		return nil, errors.New("Error: Missing Opening Key")
	}

	var elseArray []*Body
	elseBody, err := p.Body(n.depth, elseArray)
	if err != nil {
		return elseBody, err
	}

	_, err, isRightKey := p.match(finlex.TokType('}'))
	if err != nil { //EMPTY, no ha encontrado nada
		return elseBody, err
	}
	if !isRightKey && err == nil { //Hay token pero no es }
		return elseBody, errors.New("Error: Missing Closing Key")
	}

	return elseBody, nil
}

//<PARAMS> ::= <EXPR> <OTHERPARAMS>
func (p *Parser) Params() (Node []*Expr, e error) {
	p.pushTrace("Params")
	defer p.popTrace()

	var params []*Expr

	newExprNode := NewExpr()
	expr, err := p.Expr(newExprNode)
	if err != nil {
		return params, err
	}
	params = append(params, expr)

	otherparams, err := p.Otherparams(params)
	if err != nil {
		return otherparams, err
	}
	return otherparams, nil
}

//<OTHERPARAMS> ::=  ',' <Expr> <OTHERPARAMS> |
//                  empty
func (p *Parser) Otherparams(params []*Expr) (Node []*Expr, e error) {
	p.pushTrace("OtherParams")
	defer p.popTrace()
	_, err, isComa := p.match(finlex.TokType(','))
	if err != nil || !isComa { //EMPTY
		return params, err
	}

	exprNode := NewExpr()
	expr, err := p.Expr(exprNode)
	if err != nil {
		return params, err
	}
	params = append(params, expr)

	otherparams, err := p.Otherparams(params)
	if err != nil {
		return params, err
	}

	params = otherparams
	return params, nil
}

//<DECL> ::= id ':' '=' <Expr>
func (p *Parser) Decl() (Node *Asig, e error) {
	p.pushTrace("Decl")
	defer p.popTrace()

	asignode := NewAsig()
	asignode.c = "Asig"

	tok, err, isId := p.match(finlex.TokId)
	if err != nil { //EMPTY, no ha encontrado nada
		return asignode, err
	}
	if !isId && err == nil {
		return asignode, errors.New("Error: Missing Id in declaration")
	}

	asignode.id = tok.GetLexeme()
	//Lo guardo en la pila
	s := p.stkEnv.NewSymb(asignode.id, gxsymbols.Svar)
	if s == nil {
		return nil, errors.New("Stack error: Id already defined " + asignode.id)
	}

	_, err, isPoints := p.match(finlex.TokType(':'))
	if err != nil { //EMPTY, no ha encontrado nada
		return asignode, err
	}
	if !isPoints && err == nil {
		return asignode, errors.New("Error: Missing ':' in declaration")
	}
	_, err, isEqual := p.match(finlex.TokType('='))
	if err != nil { //EMPTY, no ha encontrado nada
		return asignode, err
	}
	if !isEqual && err == nil {
		return asignode, errors.New("Error: Missing '=' in declaration")
	}

	exprNode := NewExpr()
	expr, er := p.Expr(exprNode)
	if er != nil {
		return asignode, er
	}
	asignode.expr = expr

	return asignode, nil
}

//<EXPR> ::= <EXPR_1> <SIMB_1>
func (p *Parser) Expr(n *Expr) (Node *Expr, e error) {
	p.pushTrace("Expr")
	defer p.popTrace()

	node, err := p.Expr_1(n)
	if err != nil {
		return node, err
	}
	newnode, er := p.Simb_1(node)
	if er != nil {
		return newnode, er
	}
	return newnode, nil
}

//<EXPR_1> ::= <EXPR_2> <SIMB_2>
func (p *Parser) Expr_1(n *Expr) (Node *Expr, e error) {
	p.pushTrace("Expr_1")
	defer p.popTrace()

	node, err := p.Expr_2(n)
	if err != nil {
		return node, err
	}
	newnode, e := p.Simb_2(node)
	if e != nil {
		return newnode, e
	}
	return newnode, nil
}

//<SIMB_1> ::= '<=' <EXPR> |
//             '>=' <EXPR> |
//             '<'  <EXPR> |
//             '>' <EXPR>  |
//             empty
func (p *Parser) Simb_1(n *Expr) (Node *Expr, e error) {
	p.pushTrace("Simb_1")
	defer p.popTrace()

	_, err, isMinor := p.match(finlex.TokType('<'))
	if err != nil || !isMinor {
		_, err, isMajor := p.match(finlex.TokType('>'))
		if err != nil || !isMajor {
			_, err, isMajorThan := p.match(finlex.TokMajor)
			if err != nil || !isMajorThan {
				_, err, isMinorThan := p.match(finlex.TokMinor)
				if err != nil || !isMinorThan {
					return n, err
				} else {
					n.op = "<="
				}
			} else {
				n.op = ">="
			}
		} else {
			n.op = ">"
		}
	} else {
		n.op = "<"
	}
	newNode := NewExpr()
	expr, err := p.Expr(newNode)
	if err != nil {
		return n, err
	}
	n.right = append(n.right, expr)

	return n, nil
}

//<EXPR_2> ::= <EXPR_3> <SIMB_3>
func (p *Parser) Expr_2(n *Expr) (Node *Expr, e error) {
	p.pushTrace("Expr_2")
	defer p.popTrace()

	node, err := p.Expr_3(n)
	if err != nil {
		return node, err
	}
	exprNode, er := p.Simb_3(node)
	if er != nil {
		return exprNode, er
	}
	return exprNode, nil
}

//<SIMB_2> ::= '+' <EXPR_1> |
//             '-' <EXPR_1> |
//             '^'  <EXPR_1> |
//             '|' <EXPR_1>  |
//             empty
func (p *Parser) Simb_2(n *Expr) (Node *Expr, e error) {
	p.pushTrace("Simb_2")
	defer p.popTrace()

	_, err, isPlus := p.match(finlex.TokType('+'))
	if err != nil || !isPlus {
		_, err, isMinus := p.match(finlex.TokType('-'))
		if err != nil || !isMinus {
			_, err, isCircum := p.match(finlex.TokType('^'))
			if err != nil || !isCircum {
				_, err, isBar := p.match(finlex.TokType('|'))
				if err != nil || !isBar {
					return n, err
				} else { // Es |
					n.op = "|"
				}
			} else { // ES ^
				n.op = "^"
			}
		} else { // Es -
			n.op = "-"
		}
	} else { // Es +
		n.op = "+"
	}
	newNode := NewExpr()
	exprNode, err := p.Expr_1(newNode)
	if err != nil {
		return n, err
	}
	n.right = append(n.right, exprNode)
	return n, nil
}

//<EXPR_3> ::= <ATOM> <SIMB_4>
func (p *Parser) Expr_3(n *Expr) (Node *Expr, e error) {
	p.pushTrace("Expr_3")
	defer p.popTrace()

	node, err := p.Atom(n)
	if err != nil {
		return node, err
	}
	newNode, er := p.Simb_4(node)
	if er != nil {
		return newNode, er
	}
	return newNode, nil
}

//<SIMB_3> ::= '*' <EXPR_2> |
//             '/' <EXPR_2> |
//             '%' <EXPR_2> |
//             '&' <EXPR_2> |
//             empty
func (p *Parser) Simb_3(n *Expr) (Node *Expr, e error) {
	p.pushTrace("Simb_3")
	defer p.popTrace()
	_, err, isMult := p.match(finlex.TokType('*'))
	if err != nil || !isMult {
		_, err, isDiv := p.match(finlex.TokType('/'))
		if err != nil || !isDiv {
			_, err, isPercent := p.match(finlex.TokType('%'))
			if err != nil || !isPercent {
				_, err, isAmper := p.match(finlex.TokType('&'))
				if err != nil || !isAmper {
					return n, err
				} else {
					n.op = "&"
				}
			} else {
				n.op = "%"
			}
		} else {
			n.op = "/"
		}
	} else {
		n.op = "*"
	}
	newNode := NewExpr()
	exprNode, e := p.Expr_2(newNode)
	if e != nil {
		return n, e
	}
	n.right = append(n.right, exprNode)
	return n, nil
}

//<SIMB_4> ::=  '**' <EXPR_3> |
//              '!' <EXPR_3> |
//              empty
func (p *Parser) Simb_4(n *Expr) (Node *Expr, e error) {
	p.pushTrace("Simb_4")
	defer p.popTrace()
	_, err, isMult := p.match(finlex.TokAst)
	if err != nil || !isMult {
		_, err, isDiv := p.match(finlex.TokType('!'))
		if err != nil || !isDiv {
			return n, err
		} else {
			n.op = "!"
		}
	} else {
		n.op = "**"
	}

	newNode := NewExpr()
	exprNode, e := p.Expr_3(newNode)
	if e != nil {
		return n, e
	}
	n.right = append(n.right, exprNode)

	return n, nil
}

//<ATOM> ::= bool |
//           id |
//           int |
//           var_id <FIELD> |
//           '[' <PARAMS> ']' | ->> pp = [3, 35]
//           '(' <EXPR> ')'

func (p *Parser) Atom(n *Expr) (Node *Expr, e error) {
	p.pushTrace("Atom")
	defer p.popTrace()

	tokbool, err, isBool := p.match(finlex.TokBool)
	switch {
	case err != nil || !isBool: //No es bool
		toknum, err, isNum := p.match(finlex.TokType('n'))
		switch {
		case err != nil || !isNum: //No es un num
			tokid, err, isId := p.match(finlex.TokId)
			switch {
			case err != nil || !isId: //No es id
				_, err, isClasp := p.match(finlex.TokType('['))
				switch {
				case err != nil || !isClasp: //Tampoco es '[' <PARAMS> ']'
					//Compruebo si es ( <EXPR> )
					_, err, isBracket := p.match(finlex.TokType('('))
					if err != nil || !isBracket {
						return n, err
					}
					exprNode := NewExpr()
					expr, err := p.Expr(exprNode)
					if err != nil {
						return n, err
					}
					_, err, isClosingBracket := p.match(finlex.TokType(')'))
					if err != nil || !isClosingBracket {
						return n, errors.New("Error: Missing Closing bracket")
					}
					n.left = append(n.left, expr)

				default: // Si es '[' <PARAMS> ']', compruebo
					params, err := p.Params()
					if err != nil {
						return n, err
					}
					_, err, isClosingClasp := p.match(finlex.TokType(']'))
					if err != nil || !isClosingClasp {
						return n, errors.New("Error: Missing Closing Clasp ']'")
					}
					n.left = params
				}
			default: //Si es id -> var_id <FIELD>
				//Compruebo si está en la pila
				s := p.stkEnv.GetSymb(tokid.GetLexeme())
				if s == nil {
					return nil, errors.New("Stack error: Id not declared " + tokid.GetLexeme())
				}

				var field []string
				fieldnode, err := p.Field(field)
				if err != nil { //Compruebo si es var_id <FIELD>
					return n, err
				}
				exprNode := NewExpr()
				newField := tokid.GetLexeme()
				if len(fieldnode) != 0 {
					newField += "."
					newField += strings.Join(fieldnode, ".")
				}
				exprNode.leaf = newField
				exprNode.isLeaf = true //Es el final, no tiene más hijos
				n.left = append(n.left, exprNode)
			}
		default: //Es un num
			exprNode := NewExpr()
			exprNode.leaf = toknum.GetLexeme()
			exprNode.isLeaf = true // Es el final, no tiene más hijos
			n.left = append(n.left, exprNode)

		}
	default: //Es un bool
		exprNode := NewExpr()
		exprNode.leaf = tokbool.GetLexeme()
		exprNode.isLeaf = true //Es el final, no tiene más hijos
		n.left = append(n.left, exprNode)
	}
	return n, nil
}
