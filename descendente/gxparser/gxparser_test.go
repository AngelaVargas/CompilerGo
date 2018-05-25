package gxparser

import (
	"descendente/finlex"
	"fmt"
	"testing"
)

func TestParser(t *testing.T) {
	l, err := finlex.NewLexer("lang1.fx")
	if err != nil {
		t.Error(err)
	} else {
		p := NewParser(l)
		if err != nil {
			t.Error(err)
		} else {
			node, err := p.Parse()
			if err != nil {
				t.Error(err)
			}
			//Finalmente imprimo el árbol si el test ha ido bien
			fmt.Println(node.PrintTree())
		}
	}
}

func TestFinalParser(t *testing.T) {
	l, err := finlex.NewLexer("lang_fin.fx")
	if err != nil {
		t.Error(err)
	} else {
		p := NewParser(l)
		if err != nil {
			t.Error(err)
		} else {
			node, err := p.Parse()
			if err != nil {
				t.Error(err)
			}
			//Finalmente imprimo el árbol si el test ha ido bien
			fmt.Println(node.PrintTree())
		}
	}
}
