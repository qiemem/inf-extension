ifeq ($(origin NETLOGO), undefined)
	NETLOGO=../..
endif

ifeq ($(origin SCALA_HOME), undefined)
	SCALA_HOME=../..
endif

ifneq (,$(findstring CYGWIN,$(shell uname -s)))
	COLON=\;
	SCALA_HOME := `cygpath -up "$(SCALA_HOME)"`
else
	COLON=:
endif


SRCS=$(shell find src -type f -name '*.scala')
EXT_NAME=inf

JAR_REPO=http://ccl.northwestern.edu/devel/

$(EXT_NAME).jar $(EXT_NAME).jar.pack.gz: $(SRCS) manifest.txt Makefile
	mkdir -p classes
	$(SCALA_HOME)/bin/scalac -deprecation -unchecked -encoding us-ascii -classpath $(NETLOGO)/NetLogo.jar -d classes $(SRCS)
	jar cmf manifest.txt $(EXT_NAME).jar -C classes .
	pack200 --modification-time=latest --effort=9 --strip-debug --no-keep-file-order --unknown-attribute=strip $(EXT_NAME).jar.pack.gz $(EXT_NAME).jar

$(EXT_NAME).zip: $(EXT_NAME).jar
	rm -rf $(EXT_NAME)
	mkdir $(EXT_NAME)
	cp -rp $(EXT_NAME).jar $(EXT_NAME).jar.pack.gz README.md Makefile src manifest.txt $(EXT_NAME)
	zip -rv $(EXT_NAME).zip $(EXT_NAME)
	rm -rf $(EXT_NAME)
