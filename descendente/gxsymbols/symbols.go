package gxsymbols

import (
	"fmt"
)

type SymbolType int

const (
	SNone SymbolType = iota
	Svar
	Sfunc
)

var Debug bool = false

type Symb struct {
	name     string
	simbType SymbolType
}

type Env map[string]*Symb

type StkEnv []Env

func (envs *StkEnv) PushEnv() {
	env := Env{}
	*envs = append(*envs, env)
}

func (envs *StkEnv) PopEnv() {
	eS := *envs
	if len(eS) == 1 {
		panic("Cannot pop bultin")
	}
	*envs = eS[:len(eS)-1]
}

func (envs *StkEnv) NewSymb(name string, symbType SymbolType) *Symb {
	eS := *envs
	s := &Symb{name: name}
	e := eS[len(eS)-1]
	if _, ok := e[name]; ok {
		return nil
	}
	e[name] = s
	if Debug {
		fmt.Println("Stack: added symbol ", name)
	}
	return s
}

func NewEnvStk() *StkEnv {
	env := StkEnv{}
	env.PushEnv()

	env.NewSymb("circle", Sfunc)
	env.NewSymb("rect", Sfunc)

	return &env
}

func (envs *StkEnv) GetSymb(name string) (s *Symb) {
	eS := *envs
	for i := len(eS) - 1; i >= 0; i-- {
		if s, ok := eS[i][name]; ok {
			return s
		}
	}
	return nil
}
