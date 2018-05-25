package finlex

import (
	"testing"
)

func TestLangFin(t *testing.T) {
	l, err := NewLexer("lang_fin.fx")
	if err != nil {
		t.Error(err)
	} else {
		token, err := l.Lex()
		for ; token.tokType != 1; token, err = l.Lex() { //EOF=1
			if err != nil {
				t.Error(err)
			}
		}
	}
}

func TestLang(t *testing.T) {
	l, err := NewLexer("lang1.fx")
	if err != nil {
		t.Error(err)
	} else {
		token, err := l.Lex()
		for ; token.tokType != 1; token, err = l.Lex() { //EOF=1
			if err != nil {
				t.Error(err)
			}
		}
	}
}
