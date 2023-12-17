all: sa sai

#
# Satie examples
#

sa: ackermann color test todo tribute

ackermann: ../bin/sac
	../bin/sac < ackermann.sa

color: ../bin/sac
	../bin/sac < color.sa

test: ../bin/sac
	../bin/sac < test.sa

todo: ../bin/sac
	../bin/sac < todo.sa

tribute: ../bin/sac
	../bin/sac < tribute.sa

../bin/sac:
	(cd ../src/sac; $(MAKE))

#
# Satie register machine examples
#

sai: hello_world ackermannr


hello_world: ../bin/sa
	../bin/sa hello_world 0

ackermannr: ../bin/sa
	../bin/sa ackermannr 0 3 6

../bin/sa:
	(cd ../src/sa; $(MAKE))
