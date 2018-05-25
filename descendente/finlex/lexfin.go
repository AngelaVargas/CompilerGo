package finlex

import (
	"bufio"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
	"unicode"
)

type RuneScanner interface {
	ReadRune() (r rune, size int, err error)
	UnreadRune() error
}

type Lexer struct {
	file     string
	line     int
	r        RuneScanner
	lastrune rune
	peek     *Token

	accepted []rune
}

type Token struct {
	lexema      string
	tokType     TokType
	tokIntVal   int64
	tokFloatVal float64
	line        int
}

type TokType rune

const (
	RuneEof     = 0
	TokEof      = 1
	TokId       = TokType('s')
	TokArgType  = TokType('a')
	TokFunc     = TokType('f')
	TokDataType = TokType('t')
	TokRecord   = TokType('r')
	TokBool     = TokType('b')
	TokIter     = TokType('i')
	TokIntVal   = 0
	TokFloatVal = 0
	TokBoolVal  = 0
	TokIf       = TokType('w')
	TokElse     = TokType('e')
	TokMinor    = TokType('l')
	TokMajor    = TokType('p')
	TokAst      = TokType('c')
)

func (l *Lexer) Peek() (t *Token, e error) {
	if l.peek == nil {
		token, _ := l.Lex()
		l.peek = &token
	}
	return l.peek, nil
}

func (t Token) String() {
	Debug := false
	if Debug {
		fmt.Println("Lex: ['", t.lexema, "']")
	}
}

func (t *Token) Type() (tT TokType) {
	return t.tokType
}

func (t *Token) GetLexeme() (lex string) {
	return t.lexema
}

func (t *Token) GetLine() (lex int) {
	return t.line
}

func NewLexer(file string) (l *Lexer, err error) {
	l = &Lexer{line: 1}
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	l.file = file
	l.r = bufio.NewReader(f)
	return l, nil
}

func (l *Lexer) accept() (tok string) {
	tok = string(l.accepted)
	if tok == "" && l.lastrune != RuneEof {
		panic(errors.New("empty token"))
	}
	l.accepted = nil
	return tok
}

func (l *Lexer) get() (r rune) {
	var err error
	r, _, err = l.r.ReadRune()
	if err == nil {
		l.lastrune = r
	}
	if r == '\n' {
		l.line++
	}
	if err == io.EOF {
		l.lastrune = RuneEof
		return RuneEof
	}
	if err != nil {
		panic(err)
	}
	l.accepted = append(l.accepted, r)
	return r
}

func (l *Lexer) unget() {
	var err error
	if l.lastrune == RuneEof {
		return
	}
	err = l.r.UnreadRune()
	if err == nil && l.lastrune == '\n' {
		l.line++
	}
	l.lastrune = unicode.ReplacementChar
	if len(l.accepted) != 0 {
		l.accepted = l.accepted[0 : len(l.accepted)-1]
	}
	if err != nil {
		panic(err)
	}
}

func (l *Lexer) lexId() (t Token, err error) {
	r := l.get()
	if !unicode.IsLetter(r) {
		return t, errors.New("bad Id, should not happen")
	}
	isAlpha := func(ar rune) bool {
		return unicode.IsDigit(ar) || unicode.IsLetter(ar) || r == '-'
	}
	for r = l.get(); isAlpha(r); r = l.get() {
	}
	l.unget()
	t.lexema = l.accept()
	switch t.lexema {
	case "func":
		t.tokType = TokFunc
	case "iter":
		t.tokType = TokIter
	case "type":
		t.tokType = TokDataType
	case "record":
		t.tokType = TokRecord
	case "if":
		t.tokType = TokIf
	case "else":
		t.tokType = TokElse
	case "int":
		t.tokType = TokArgType
	case "bool":
		t.tokType = TokArgType
	case "Coord":
		t.tokType = TokArgType
	case "True":
		t.tokType = TokBool
	case "False":
		t.tokType = TokBool
	default:
		t.tokType = TokId
	}
	return t, nil
}

func (l *Lexer) lexNum() (t Token, err error) {
	const (
		Es    = "Ee"
		Signs = "+−"
	)
	hasDot := false
	hexnum := false
	r := l.get()
	if r == '.' {
		hasDot = true
		r = l.get()
	}
	if r == '0' {
		r = l.get()
		if r == 'x' {
			hexnum = true // Tengo que comprobar si es un hexadecimal 0xfffff7
			r = l.get()
		}
	}
	for ; unicode.IsDigit(r) || unicode.IsLetter(r); r = l.get() {
	}
	if r == '.' {
		if hasDot {
			return t, errors.New("bad float [" + l.accept() + "]")
		}
		hasDot = true
		for r = l.get(); unicode.IsDigit(r); r = l.get() {
		}
	}
	switch {
	case strings.ContainsRune(Es, r): //tiene un exponencial

	case hasDot: //tiene un punto, será un float
		l.unget()
		break
	case hexnum:
		l.unget()
		t.lexema = l.accept()
		t.tokIntVal = TokIntVal
		_, err := hex.DecodeString(t.lexema[2:]) //Compruebo si el num hex guardado realmente lo es, pero sin 0x
		if err != nil {
			return t, errors.New("bad hex [" + t.lexema + "]")
		}
		t.tokType = TokType('n')
		return t, nil
	case !hasDot: //es un int
		l.unget()
		t.lexema = l.accept()
		t.tokIntVal, err = strconv.ParseInt(t.lexema, 10, 64)
		if err != nil {
			return t, errors.New("bad int [ " + t.lexema + "]")
		}
		t.tokType = TokType('n')
		return t, nil
	default:
		return t, errors.New("bad float [ " + l.accept() + "]")
	}
	r = l.get()
	if strings.ContainsRune(Signs, r) {
		r = l.get()
	}
	for ; unicode.IsDigit(r); r = l.get() {
	}
	l.unget()
	t.lexema = l.accept()
	t.tokFloatVal, err = strconv.ParseFloat(t.lexema, 64)
	if err != nil {
		return t, errors.New("bad float [ " + t.lexema + "]")
	}
	t.tokType = TokType('n')
	return t, nil
}

func (l *Lexer) Lex() (t Token, err error) {
	if l.peek != nil {
		t = *l.peek
		l.peek = nil
		return t, nil
	}
	for r := l.get(); ; r = l.get() {
		if r == '/' && l.get() == '/' { //Es un comentario
			for r = l.get(); r != '\n'; r = l.get() { //Hasta que llegue al final de línea, ignorar
			}
			l.accept()
			continue //Nueva vuelta
		}
		if unicode.IsSpace(r) && r != '\n' {
			l.accept()
			continue //Nueva vuelta al bucle
		}
		if r == '\n' {
			l.accept()
			continue
		}
		if r == '<' && l.get() == '=' { //Es <=
			t.tokType = TokMinor
			t.lexema = l.accept()
			t.line = l.line
			return t, nil
		}
		if r == '>' && l.get() == '=' { //Es >=
			t.tokType = TokMajor
			t.lexema = l.accept()
			t.line = l.line
			return t, nil
		}
		/*if r == '*' && l.get()=='*'{    //Es **
		  t.tokType = TokAst
		  t.lexema = l.accept()
		  t.line = l.line
		  return t, nil
		}*/

		switch r {
		case '+', '-', '*', '/', '>', '<', '=', '|', '&', '!', '^', '.', ':', ',', ';', '%':
			t.tokType = TokType(r) //Añado cada signo de puntuación al tipo TokType, hay que declarar el tipo TokType
			t.lexema = l.accept()
			t.line = l.line
			return t, nil
		case '(', ')', '[', ']', '{', '}':
			t.tokType = TokType(r) //Añado cada signo de puntuación al tipo TokType, hay que declarar el tipo TokType
			t.lexema = l.accept()
			t.line = l.line
			return t, nil
		case RuneEof:
			t.tokType = TokEof
			t.line = l.line
			l.accept()
			return t, nil
		}
		switch {
		case r == '.', unicode.IsDigit(r):
			l.unget()
			t, err = l.lexNum()
			t.line = l.line
			return t, err
		case unicode.IsLetter(r):
			l.unget()
			t, err = l.lexId()
			t.line = l.line
			return t, err
		default:
			errs := fmt.Sprintf("bad rune %c: %x", r, r)
			t.line = l.line
			return t, errors.New(errs)
		}
	}
	t.line = l.line
	return t, err
}
