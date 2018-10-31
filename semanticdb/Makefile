path="input/src/main/scala/"
files=$(shell find $(path) -type f -name '*.scala')

all: %.tasty
	cd input; sbt compile

test: all
	cd ..; sbt dotty-semanticdb/test; cd -

%.tasty: $(files)
	cd $(dir $^); $(CURDIR)/../bin/dotc $(notdir $^)
