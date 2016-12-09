SBT = sbt

build:
	${SBT} compile

test:
	${SBT} test

console:
	${SBT} test:console

clean:
	${SBT} clean

.PHONY: build test console clean

