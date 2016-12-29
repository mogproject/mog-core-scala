SBT = sbt

build:
	${SBT} compile

test:
	${SBT} test

console:
	${SBT} mogCoreJVM/test:console

clean:
	${SBT} clean

.PHONY: build test console clean

