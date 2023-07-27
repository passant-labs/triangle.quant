
.PHONY: test


SBT = sbt -J-Xms512M -J-Xmx1G -J-Xss1M $(SCALA_OPTIONS)

clean:
	$(SBT) clean

compile:
	$(SBT) compile

package:
	$(SBT) package

build:
	$(MAKE) clean compile package

repl:
	$(SBT)

GIT_BRANCH ?= main
code/pull:
	git submodule foreach git pull --rebase
	git pull --rebase origin $(GIT_BRANCH)

run:
	$(SBT) "runMain chaospi.triangle.$(APP)"

launch:
	$(SBT) "runMain extension.Launch chaospi.triangle.$(APP)"

setup:
	$(SBT) "runMain extension.Launch chaospi.triangle.Setup"

hedge:
	$(MAKE) run

test/launch:
	$(SBT) "test:runMain extension.Launch chaospi.triangle.$(APP)"



