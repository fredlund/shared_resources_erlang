JAVASOURCES = $(notdir $(wildcard *.java))

### Locate a recent aedlib.jar (see for instance http://costa.ls.fi.upm.es/entrega/aed/aedlib/) and a cclib
LIBDIR = /home/fred/svns/courses/cc/2019-2020-s2/practicas/codigo/libs

CLASSPATH = classes:$(LIBDIR)/aedlib-2.8.0.jar:$(LIBDIR)/jcsp.jar:$(LIBDIR)/cclib-0.4.9.jar:$(LIBDIR)/sequenceTester.jar:$(LIBDIR)/junit-jupiter-api-5.6.2.jar:$(LIBDIR)/opentest4j-1.2.0.jar:$(LIBDIR)/apiguardian-api-1.0.0.jar

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
