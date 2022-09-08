package main

import (
	"fmt"
	"os"
	"sort"
	"strings"
	"time"
)

func readLine(path string) []string {
	b, _ := os.ReadFile(path)
	str := string(b)
	return strings.Split(str, "\n")
}

type Team = uint8

const (
	Immunte Team = iota
	Infection
)

type unit struct {
	team  Team
	count int
	hp    int

	// Contains the weak to and immune to items
	attr map[string][]string

	attack     int
	attackType string
	initiative int
	attacked   bool
	dead       bool
}

func (u unit) String() string {
	team := "Immune"
	if u.team == Infection {
		team = "Infection"
	}
	return fmt.Sprintf("(team: %s, units: %d, attack: %d, ep: %d, init: %d)", team, u.count, u.attack, effectivePower(u), u.initiative)
}

func effectivePower(unit unit) int {
	return unit.attack * unit.count
}

func s(u1, u2 unit) bool {
	ue1 := effectivePower(u1)
	ue2 := effectivePower(u2)

	if ue1 == ue2 {
		return u1.initiative > u2.initiative
	}
	return ue1 > ue2
}

func split(line string) (string, string, string) {
	paren := strings.Index(line, "(")
	parenClose := strings.Index(line, ")")

	if paren == -1 {
		points := "points"
		next := strings.Index(line, points)

		n1 := line[:next+len(points)+1]
		n3 := line[next+len(points)+1:]

		return n1, "", n3

	} else {
		return line[:paren], line[paren+1 : parenClose], line[parenClose+2:]
	}
}
func parseUnit(line string, team Team, boost int) unit {
	unit := unit{}

	fst, snd, trd := split(line)

	fmt.Sscanf(fst, "%d units each with %d hit points", &unit.count, &unit.hp)
	fmt.Sscanf(trd, "with an attack that does %d %s damage at initiative %d", &unit.attack, &unit.attackType, &unit.initiative)

	spl := strings.Split(snd, ";")

	attr := map[string][]string{"weak": {}, "immune": {}}
	for _, sp := range spl {
		if len(sp) == 0 {
			continue
		}
		sp = strings.Trim(sp, ";")
		typ := strings.Split(strings.TrimLeft(sp, " "), " ")[0]

		l := strings.Split(sp[strings.Index(sp, "to")+3:], ", ")

		attr[typ] = l
	}
	unit.attr = attr
	unit.team = team
	unit.attacked = false
	unit.dead = false
	if unit.team == Immunte {
		unit.attack += boost
	}

	return unit
}

func bothSideAlive(us []unit) bool {
	b1 := false
	b2 := false

	for _, v := range us {
		if v.dead {
			continue
		}

		if v.team == Infection {
			b1 = true
		} else if v.team == Immunte {
			b2 = true
		}
		if b1 && b2 {
			return true
		}
	}
	return false
}

func reset(units *[]unit) {
	for i := 0; i < len(*units); i++ {
		(*units)[i].attacked = false
	}
}

func contains[T comparable](t T, ts []T) bool {
	for _, v := range ts {
		if v == t {
			return true
		}
	}
	return false
}

func attackSort(u1, u2, me unit) bool {
	ep := effectivePower(me)

	au1 := ep
	if contains(me.attackType, u1.attr["weak"]) {
		au1 *= 2
	}
	au2 := ep
	if contains(me.attackType, u2.attr["weak"]) {
		au2 *= 2
	}

	if au1 == au2 {
		ep1 := effectivePower(u1)
		ep2 := effectivePower(u2)
		if ep1 == ep2 {
			return u1.initiative > u2.initiative

		} else {
			return ep1 > ep2
		}

	} else {
		return au1 > au2
	}
}

func find(init int, us []unit) int {
	for i, v := range us {
		if v.initiative == init {
			return i
		}
	}
	return -1
}

func unitsLost(attack, defend unit) int {
	ep := effectivePower(attack)
	if contains(attack.attackType, defend.attr["weak"]) {
		ep *= 2
	}

	res := ep / defend.hp
	if res > defend.count {
		res = defend.count
	}
	return res
}

type attack struct {
	initiative int
	attacker   int
	defender   int
}

func canAttack(attacker, defender unit) bool {
	return attacker.team != defender.team && !defender.attacked && !contains(attacker.attackType, defender.attr["immune"]) && !defender.dead
}

func fight(array []string, boost int) (int, Team) {
	idx := 1

	units := []unit{}

	for ; len(array[idx]) > 0; idx++ {
		units = append(units, parseUnit(array[idx], Immunte, boost))
	}

	idx += 2
	for ; idx < len(array) && len(array[idx]) > 0; idx++ {
		units = append(units, parseUnit(array[idx], Infection, 0))
	}

	for bothSideAlive(units) {
		reset(&units)

		sort.Slice(units, func(i, j int) bool { return s(units[i], units[j]) })

		// selection phase
		attacks := []attack{}
		for ai, attacker := range units {
			if attacker.dead {
				continue
			}

			target := -1

			for di, defender := range units {
				if canAttack(attacker, defender) {
					if target == -1 || !attackSort(units[target], units[di], attacker) {
						target = di
					}
				}
			}

			if target != -1 {
				units[target].attacked = true
				attacks = append(attacks, attack{initiative: attacker.initiative, attacker: ai, defender: target})
			}
		}

		// Attack phase
		lostUnits := 0
		sort.Slice(attacks, func(i, j int) bool { return attacks[i].initiative > attacks[j].initiative })

		for _, attack := range attacks {
			if units[attack.attacker].dead {
				continue
			}

			lost := unitsLost(units[attack.attacker], units[attack.defender])
			lostUnits += lost
			units[attack.defender].count -= lost
			if units[attack.defender].count <= 0 {
				units[attack.defender].dead = true
			}
		}

		// stalemate
		if lostUnits == 0 {
			return -1, Infection
		}
	}
	count := 0
	var team Team
	for _, v := range units {
		if !v.dead || v.count > 0 {
			count += v.count
			team = v.team
		}
	}

	return count, team
}

func partOne(array []string) int {
	hp, _ := fight(array, 0)
	return hp
}

func partTwo(array []string) int {
	for i := 1; ; i++ {
		hp, team := fight(array, i)
		if team == Immunte {
			return hp
		}
	}
}

type Task int

const (
	Silver Task = 0
	Gold        = 1
)

func timeFunc[T any, S any](task Task, f func(T) S, arg T) {

	t0 := time.Now()
	res := f(arg)
	elapsed := time.Now().Sub(t0).Milliseconds()

	switch task {
	case Silver:
		fmt.Printf("(%dms)\tTask one: \x1b[0;34;34m%v\x1b[0m\n", elapsed, res)
	case Gold:
		fmt.Printf("(%dms)\tTask two: \x1b[0;33;10m%v\x1b[0m\n", elapsed, res)
	}
}

func main() {
	args := os.Args
	if len(args) != 2 {
		fmt.Println("Usage: ./main <input file>")
		os.Exit(-1)
	}

	array := readLine(args[1])
	timeFunc(Silver, partOne, array)
	timeFunc(Gold, partTwo, array)
}
