all: graphgrow-audio graphgrow-audio-test

clean:
	-rm graphgrow-audio graphgrow-audio-test

graphgrow-audio: graphgrow-audio.cc
	g++ -std=c++11 -Wall -Wextra -pedantic -Wno-variadic-macros -O3 -march=native -ffast-math -I$(HOME)/opt/include -L$(HOME)/opt/lib -o graphgrow-audio graphgrow-audio.cc -ljack -llo

graphgrow-audio-test: graphgrow-audio-test.cc
	g++ -std=c++11 -Wall -Wextra -pedantic -Wno-variadic-macros -O3 -march=native -ffast-math -I$(HOME)/opt/include -L$(HOME)/opt/lib -o graphgrow-audio-test graphgrow-audio-test.cc -llo
