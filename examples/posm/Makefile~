all: ackermann color test todo tribute

ackermann: satie
	../../grammar/satie < ackermann.sa

color: satie
	../../grammar/satie < color.sa

test: satie
	../../grammar/satie < test.sa

todo: satie
	../../grammar/satie < todo.sa


tribute: satie
	../../grammar/satie < tribute.sa

satie:
	(cd ../../grammar; make)

.PHONY: satie all
