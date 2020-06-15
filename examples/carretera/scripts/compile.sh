#!/bin/sh

if [ $# -lt 1 ]
then
	echo "Usage: $0 entrega-dirs"
	exit 1
fi

CMD=`basename $0`
PRG=$0

while [ -L "${PRG}" ]; do
    ls=`ls -ld "${PRG}"`
    link=`expr "$ls" : '.*-> \(.*\)$'`
    if expr "${link}" : '\/' > /dev/null; then
        PRG="${link}"
    else
        PRG="`dirname ${PRG}`/${link}"
    fi
done

SCRIPTHOME=`dirname "${PRG}"`/..

EntregaDirs=$1
shift

LIBDIR=/home/fred/svns/courses/cc/2019-2020-s2/practicas/codigo/libs

CLASSPATH=$LIBDIR/../classes:$LIBDIR/aedlib-2.8.0.jar:$LIBDIR/jcsp.jar:$LIBDIR/cclib-0.4.9.jar:$LIBDIR/sequenceTester.jar:$LIBDIR/junit-jupiter-api-5.6.2.jar:$LIBDIR/opentest4j-1.2.0.jar:$LIBDIR/apiguardian-api-1.0.0.jar

for dir in $EntregaDirs/*/
do
    bash $SCRIPTHOME/scripts/compileSolution.sh "$dir"
done
