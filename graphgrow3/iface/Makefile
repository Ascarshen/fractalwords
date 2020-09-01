all: graphgrow-iface selected.raw deselected.raw help.raw

clean:
	-rm graphgrow-iface selected.raw deselected.raw help.raw

graphgrow-iface: graphgrow-iface.cc
	g++ -std=c++11 -Wall -Wextra -pedantic -Wno-variadic-macros -Wno-sign-compare -O3 -march=native -ffast-math -I$(HOME)/opt/include -L$(HOME)/opt/lib -o graphgrow-iface graphgrow-iface.cc -lGL -lGLEW -lglfw -llo

selected.raw: selected.png
	pngtopnm < selected.png | tail -c 192000 > selected.raw

deselected.raw: deselected.png
	pngtopnm < deselected.png | tail -c 192000 > deselected.raw

help.raw: help.png
	pngtopnm < help.png | tail -c 640000 > help.raw
