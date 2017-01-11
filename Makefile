SBT = sbt

build:
	${SBT} compile

test:
	${SBT} test

console:
	${SBT} mogCoreJVM/test:console

clean:
	${SBT} clean

bench: clean
	${SBT} mogCoreJVM/test:run 'set scalaJSStage in Global := FullOptStage' mogCoreJS/test:run

.PHONY: build test console clean bench

