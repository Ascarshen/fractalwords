all: graphgrow-video graphgrow-video-test

clean:
	-rm graphgrow-video graphgrow-video-test

graphgrow-video: graphgrow-video.cc
	g++ -std=c++11 -Wall -Wextra -pedantic -Wno-variadic-macros -O3 -march=native -ffast-math -I$(HOME)/opt/include -L$(HOME)/opt/lib -o graphgrow-video graphgrow-video.cc -lGL -lGLEW -lglfw -llo

graphgrow-video-test: graphgrow-video-test.cc
	g++ -std=c++11 -Wall -Wextra -pedantic -Wno-variadic-macros -O3 -march=native -ffast-math -I$(HOME)/opt/include -L$(HOME)/opt/lib -o graphgrow-video-test graphgrow-video-test.cc -llo
