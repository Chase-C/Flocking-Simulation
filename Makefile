CC		= ghc
FLAGS	= -O2 -Wall -odir obj -hidir obj
SRCDIR	= src
FILES	= Main.hs Boid.hs Neighborhood.hs Utils.hs Vec3D.hs
SOURCES	= $(FILES:%.hs=${SRCDIR}/%.hs)
EXEC	= Boids

all : ${EXEC}

${EXEC} : ${SOURCES}
	${CC} ${FLAGS} -o $@ $^

clean :
	- rm ${EXEC}.exe
