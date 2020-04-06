JAVASOURCES = $(notdir $(wildcard *.java))

### Locate a recent aedlib.jar (see for instance http://costa.ls.fi.upm.es/entrega/aed/aedlib/) and a cclib
CLASSPATH = /home/fred/svns/courses/aed/trunk/lib/aedlib.jar:/home/fred/Downloads/cclib-0.4.9.jar

PACKAGE = cc.carretera

Tester = CarreteraSim

CompileCommand = javac -Xlint:unchecked -d classes -cp $(CLASSPATH) *.java
RunCommand = java -cp classes:$(CLASSPATH) $(PACKAGE)/$(Tester)

all: classes TesterClass

classes:
	mkdir -p classes

TesterClass: $(JAVASOURCES)
	$(warning $(IsJunitTester))
	$(CompileCommand)

run: classes TesterClass
	$(RunCommand)
