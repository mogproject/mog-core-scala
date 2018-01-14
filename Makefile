SBT = sbt

build:
	${SBT} compile

test:
	${SBT} test

testJVM:
	${SBT} mogCoreJVM/test:test

console:
	${SBT} mogCoreJVM/test:console

clean:
	${SBT} clean

bench: clean
	${SBT} mogCoreJVM/test:run 'set scalaJSStage in Global := FullOptStage' mogCoreJS/test:run

benchJS:
	${SBT} 'set scalaJSStage in Global := FullOptStage' mogCoreJS/test:run

merge:
	git checkout master && git pull && git checkout develop && git merge master && git push

.PHONY: build test testJVM console clean bench benchJS merge

