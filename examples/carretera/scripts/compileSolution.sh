#!/bin/sh

if [ $# -lt 1 ]
then
	echo "Usage: $0 entrega-dir"
	exit 1
fi

dir=$1
shift

LIBDIR=/home/fred/svns/courses/cc/2019-2020-s2/practicas/codigo/libs

CLASSPATH=$LIBDIR/../classes:$LIBDIR/aedlib-2.8.0.jar:$LIBDIR/jcsp.jar:$LIBDIR/cclib-0.4.9.jar:$LIBDIR/sequenceTester.jar:$LIBDIR/junit-jupiter-api-5.6.2.jar:$LIBDIR/opentest4j-1.2.0.jar:$LIBDIR/apiguardian-api-1.0.0.jar

cd $dir
echo "Compiling $dir"
javac -d classes -cp $CLASSPATH *java || javac -d classes -encoding windows-1252 -cp $CLASSPATH *java

cd -
